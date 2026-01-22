{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs, getEnv, lookupEnv)
import System.Exit (die)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString as BS

-- TODO: Import actual modules once implemented
-- import API
-- import Store
-- import Types

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["shorten"] -> runShortenDaemon
        ["redirect"] -> runRedirectDaemon
        ["version"] -> putStrLn "hadlink v0.1.0-dev"
        _ -> die "Usage: hadlink {shorten|redirect|version}"

-- TODO: Implement shorten daemon
-- Accepts untrusted input, performs validation, writes to storage
runShortenDaemon :: IO ()
runShortenDaemon = do
    port <- getEnvWithDefault "HADLINK_PORT" "8443"
    storagePath <- getEnvWithDefault "HADLINK_STORAGE" "/var/lib/hadlink/hadlink.db"
    
    putStrLn $ "Starting shorten daemon on port " ++ port
    putStrLn $ "Storage: " ++ storagePath
    putStrLn "TODO: Implement shorten daemon"
    
    -- TODO: Initialize storage
    -- TODO: Load configuration
    -- TODO: Start HTTP server with API handlers
    -- TODO: Implement rate limiting
    -- TODO: Implement proof-of-work verification

-- TODO: Implement redirect daemon
-- Fast, read-only, performs single lookup and redirect
runRedirectDaemon :: IO ()
runRedirectDaemon = do
    port <- getEnvWithDefault "HADLINK_PORT" "8080"
    storagePath <- getEnvWithDefault "HADLINK_STORAGE" "/var/lib/hadlink/hadlink.db"
    
    putStrLn $ "Starting redirect daemon on port " ++ port
    putStrLn $ "Storage: " ++ storagePath ++ " (read-only)"
    putStrLn "TODO: Implement redirect daemon"
    
    -- TODO: Open storage in read-only mode
    -- TODO: Start HTTP server with minimal handler
    -- TODO: Single lookup -> 302 redirect
    -- TODO: No parsing, no allocation beyond lookup

-- Helper to get environment variable with default
getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault var def = fromMaybe def <$> lookupEnv var
