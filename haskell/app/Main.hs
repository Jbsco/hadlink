{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.Wai.Handler.Warp (run)
import Text.Read (readMaybe)

import API
import Store
import Types

main :: IO ()
main = do
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

    -- Initialize storage
    store <- openStore storagePath
    initializeStore store

    putStrLn "Ready to accept requests"

    -- Start HTTP server
    run port (app config store)

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

    putStrLn "Ready to redirect"

    -- Start HTTP server (only GET requests handled)
    run port (app config store)

-- | Helper to get environment variable with default
getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault var def = fromMaybe def <$> lookupEnv var
