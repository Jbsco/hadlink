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

module Logging
  ( Logger
  , newLogger
  , closeLogger
  , logInfo
  , logWarn
  , logError
  , logRequest
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson (object, (.=), encode)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request, requestMethod, rawPathInfo)
import Network.HTTP.Types (Status, statusCode)
import System.Log.FastLogger

-- | Logger wrapping a TimedFastLogger
data Logger = Logger
  { lgLogger  :: TimedFastLogger
  , lgCleanup :: IO ()
  }

-- | Create a new logger (structured JSON to stdout)
newLogger :: IO Logger
newLogger = do
  tc <- newTimeCache simpleTimeFormat
  (logger, cleanup) <- newTimedFastLogger tc (LogStdout defaultBufSize)
  return $ Logger logger cleanup

-- | Close the logger
closeLogger :: Logger -> IO ()
closeLogger = lgCleanup

-- | Log at info level
logInfo :: Logger -> T.Text -> [(T.Text, T.Text)] -> IO ()
logInfo = logAt "info"

-- | Log at warn level
logWarn :: Logger -> T.Text -> [(T.Text, T.Text)] -> IO ()
logWarn = logAt "warn"

-- | Log at error level
logError :: Logger -> T.Text -> [(T.Text, T.Text)] -> IO ()
logError = logAt "error"

-- | Internal: log at a given level
logAt :: T.Text -> Logger -> T.Text -> [(T.Text, T.Text)] -> IO ()
logAt level lg msg dat =
  lgLogger lg $ \ft ->
    let ts = TE.decodeUtf8 (fromLogStr (toLogStr ft))
        val = object $
          [ "ts"    .= ts
          , "level" .= level
          , "msg"   .= msg
          ] ++ map (\(k, v) -> Key.fromText k .= v) dat
    in toLogStr (BL.toStrict (encode val)) <> toLogStr ("\n" :: BS8.ByteString)

-- | Log an HTTP request with method, path, and status
logRequest :: Logger -> Request -> Status -> IO ()
logRequest lg req status =
  logAt "info" lg "request"
    [ ("method", TE.decodeUtf8 (requestMethod req))
    , ("path", TE.decodeUtf8 (rawPathInfo req))
    , ("status", T.pack (show (statusCode status)))
    ]
