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

import System.Environment (lookupEnv)
import System.Exit (die)
import Control.Monad (when)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)

import API.Shorten (shortenApp)
import Store
import Types
import RateLimit (newRateLimiter)
import SparkFFI (initSpark, finalizeSpark)
import Logging (newLogger, closeLogger, logInfo)

main :: IO ()
main =
    bracket initSpark (const finalizeSpark) $ \_ ->
    bracket newLogger closeLogger $ \logger -> do
    port <- read <$> getEnvWithDefault "HADLINK_PORT" "8443"
    storagePath <- getEnvWithDefault "HADLINK_STORAGE" "./hadlink.db"
    secretStr <- lookupEnv "HADLINK_SECRET"
    powDiffStr <- getEnvWithDefault "HADLINK_POW_DIFFICULTY" "0"
    powDiffAuthStr <- getEnvWithDefault "HADLINK_POW_DIFFICULTY_AUTH" "0"
    apiKeysStr <- getEnvWithDefault "HADLINK_API_KEYS" ""
    trustProxyStr <- getEnvWithDefault "HADLINK_TRUST_PROXY" "false"

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

    baseURLStr <- getEnvWithDefault "HADLINK_BASE_URL" ("http://localhost:" ++ show port)

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
          , cfgBaseURL = T.pack baseURLStr
          }

    logInfo logger "Starting shorten daemon" [("port", T.pack (show port))]
    logInfo logger "Configuration"
      [ ("storage", T.pack storagePath)
      , ("pow_difficulty", T.pack (show powDifficulty))
      , ("pow_difficulty_auth", T.pack (show powDifficultyAuth))
      , ("api_keys", T.pack (show (length apiKeys)))
      , ("trust_proxy", T.pack (show trustProxy))
      ]

    when trustProxy $
        logInfo logger "WARNING: X-Forwarded-For trusted. Only enable behind a trusted reverse proxy!" []

    store <- openStore storagePath
    initializeStore store

    limiter <- newRateLimiter config

    logInfo logger "Ready to accept requests" []

    run port (shortenApp config store limiter logger)

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
