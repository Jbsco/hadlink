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

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)

import API.Resolve (resolveApp)
import Store (openStore)
import Logging (newLogger, closeLogger, logInfo)

main :: IO ()
main =
    bracket newLogger closeLogger $ \logger -> do
    port <- read <$> getEnvWithDefault "HADLINK_PORT" "8080"
    storagePath <- getEnvWithDefault "HADLINK_STORAGE" "./hadlink.db"

    logInfo logger "Starting redirect daemon"
      [ ("port", T.pack (show port))
      , ("storage", T.pack storagePath)
      ]

    store <- openStore storagePath

    logInfo logger "Ready to redirect" []

    run port (resolveApp store logger)

-- | Helper to get environment variable with default
getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault var def = fromMaybe def <$> lookupEnv var
