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
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS8
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
    secretStr <- getEnvWithDefault "HADLINK_SECRET" "CHANGE_ME_INSECURE_DEFAULT"

    let secret = BS8.pack secretStr
        config = Config
          { cfgSecret = secret
          , cfgPowDifficulty = 0  -- Disabled by default
          , cfgRateLimitPerIP = 10
          , cfgRateLimitWindow = 60
          , cfgStoragePath = storagePath
          , cfgAPIKeys = []  -- TODO: Load from config
          }

    putStrLn $ "Starting shorten daemon on port " ++ show port
    putStrLn $ "Storage: " ++ storagePath
    putStrLn $ "Rate limit: " ++ show (cfgRateLimitPerIP config) ++ " requests per " ++ show (cfgRateLimitWindow config) ++ "s"

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
          , cfgRateLimitPerIP = 0
          , cfgRateLimitWindow = 0
          , cfgStoragePath = storagePath
          , cfgAPIKeys = []
          }

    -- Initialize rate limiter (not used by redirect, but required by app signature)
    limiter <- newRateLimiter config

    putStrLn "Ready to redirect"

    -- Start HTTP server (only GET requests handled)
    run port (app config store limiter)

-- | Helper to get environment variable with default
getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault var def = fromMaybe def <$> lookupEnv var
