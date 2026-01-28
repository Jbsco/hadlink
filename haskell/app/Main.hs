-- hadlink - High-assurance URL shortener
-- Copyright (C) 2026 hadlink contributors
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)

import API
import Store
import Types
import RateLimit (newRateLimiter)
import SparkFFI (initSpark)

main :: IO ()
main = do
    --  Initialize Ada runtime for SPARK FFI
    initSpark
    
    args <- getArgs
    case args of
        ["shorten"] -> runShortenDaemon
        ["redirect"] -> runRedirectDaemon
        ["version"] -> putStrLn "hadlink v0.1.0-dev"
        _ -> die "Usage: hadlink {shorten|redirect|version}"

-- | Run shorten daemon (creation service)
-- Accepts untrusted input, performs validation, writes to storage
runShortenDaemon :: IO ()
runShortenDaemon = do
    -- Get configuration from environment
    port <- read <$> getEnvWithDefault "HADLINK_PORT" "8443"
    storagePath <- getEnvWithDefault "HADLINK_STORAGE" "./hadlink.db"
    secretStr <- lookupEnv "HADLINK_SECRET"
    powDiffStr <- getEnvWithDefault "HADLINK_POW_DIFFICULTY" "0"
    powDiffAuthStr <- getEnvWithDefault "HADLINK_POW_DIFFICULTY_AUTH" "0"
    apiKeysStr <- getEnvWithDefault "HADLINK_API_KEYS" ""
    trustProxyStr <- getEnvWithDefault "HADLINK_TRUST_PROXY" "false"

    -- Validate secret: must be set and not the insecure default
    let insecureDefault = "CHANGE_ME_INSECURE_DEFAULT"
    secret <- case secretStr of
        Nothing -> die $ unlines
            [ "ERROR: HADLINK_SECRET environment variable is not set."
            , ""
            , "The shorten service requires a secret key for HMAC-based short code generation."
            , "Generate a secure secret with: openssl rand -hex 16"
            , "Then set it: export HADLINK_SECRET=<your-secret>"
            ]
        Just s | s == insecureDefault -> die $ unlines
            [ "ERROR: HADLINK_SECRET is set to the insecure default value."
            , ""
            , "You must set a unique secret key for production use."
            , "Generate a secure secret with: openssl rand -hex 16"
            , "Then set it: export HADLINK_SECRET=<your-secret>"
            ]
        Just s | null s -> die "ERROR: HADLINK_SECRET is empty. Please set a non-empty secret key."
        Just s -> return $ BS8.pack s

    let powDifficulty = read powDiffStr
        powDifficultyAuth = read powDiffAuthStr
        apiKeys = parseAPIKeys apiKeysStr
        trustProxy = map toLower trustProxyStr `elem` ["true", "1", "yes"]
        config = Config
          { cfgSecret = secret
          , cfgPowDifficulty = Difficulty powDifficulty
          , cfgPowDifficultyAuth = Difficulty powDifficultyAuth
          , cfgRateLimitPerIP = 10
          , cfgRateLimitWindow = 60
          , cfgStoragePath = storagePath
          , cfgAPIKeys = apiKeys
          , cfgTrustProxy = trustProxy
          }

    putStrLn $ "Starting shorten daemon on port " ++ show port
    putStrLn $ "Storage: " ++ storagePath
    putStrLn $ "PoW difficulty: " ++ show powDifficulty ++ " (anonymous), " ++ show powDifficultyAuth ++ " (authenticated)"
    putStrLn $ "API keys configured: " ++ show (length apiKeys)
    putStrLn $ "Rate limit: " ++ show (cfgRateLimitPerIP config) ++ " requests per " ++ show (cfgRateLimitWindow config) ++ "s"
    putStrLn $ "Trust proxy (X-Forwarded-For): " ++ show trustProxy
    when trustProxy $
        putStrLn "WARNING: X-Forwarded-For trusted. Only enable behind a trusted reverse proxy!"

    -- Initialize storage
    store <- openStore storagePath
    initializeStore store

    -- Initialize rate limiter
    limiter <- newRateLimiter config

    putStrLn "Ready to accept requests"

    -- Start HTTP server
    run port (app config store limiter)

-- | Run redirect daemon (resolution service)
-- Fast, read-only, performs single lookup and redirect
runRedirectDaemon :: IO ()
runRedirectDaemon = do
    port <- read <$> getEnvWithDefault "HADLINK_PORT" "8080"
    storagePath <- getEnvWithDefault "HADLINK_STORAGE" "./hadlink.db"

    putStrLn $ "Starting redirect daemon on port " ++ show port
    putStrLn $ "Storage: " ++ storagePath ++ " (read-only)"

    -- Open storage (read-only in practice, SQLite doesn't have true RO without filesystem perms)
    store <- openStore storagePath

    -- Minimal config for redirect-only
    let config = Config
          { cfgSecret = ""  -- Not used by redirect
          , cfgPowDifficulty = 0
          , cfgPowDifficultyAuth = 0
          , cfgRateLimitPerIP = 0
          , cfgRateLimitWindow = 0
          , cfgStoragePath = storagePath
          , cfgAPIKeys = []
          , cfgTrustProxy = False  -- Redirect daemon doesn't use rate limiting
          }

    -- Initialize rate limiter (not used by redirect, but required by app signature)
    limiter <- newRateLimiter config

    putStrLn "Ready to redirect"

    -- Start HTTP server (only GET requests handled)
    run port (app config store limiter)

-- | Helper to get environment variable with default
getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault var def = fromMaybe def <$> lookupEnv var

-- | Parse comma-separated API keys from environment
parseAPIKeys :: String -> [APIKey]
parseAPIKeys "" = []
parseAPIKeys str = map (APIKey . T.strip . T.pack) $ splitOn ',' str
  where
    splitOn :: Char -> String -> [String]
    splitOn _ "" = []
    splitOn c s = case break (== c) s of
      (token, "") -> [token]
      (token, _:rest) -> token : splitOn c rest
