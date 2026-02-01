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

module Health
  ( healthHandler
  ) where

import Store (SQLiteStore, ping)
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200, status503)
import Data.Aeson (object, (.=), encode)
import Control.Exception (try, SomeException)

-- | Health check handler
-- Returns 200 {"status":"ok"} if DB is reachable, 503 otherwise
healthHandler :: SQLiteStore -> Application
healthHandler store _req respond = do
  result <- try (ping store) :: IO (Either SomeException Bool)
  case result of
    Right True ->
      respond $ responseLBS status200
        [("Content-Type", "application/json")]
        (encode $ object ["status" .= ("ok" :: String)])
    Right False ->
      respond $ responseLBS status503
        [("Content-Type", "application/json")]
        (encode $ object
          [ "status" .= ("error" :: String)
          , "detail" .= ("database unreachable" :: String)
          ])
    Left ex ->
      respond $ responseLBS status503
        [("Content-Type", "application/json")]
        (encode $ object
          [ "status" .= ("error" :: String)
          , "detail" .= show ex
          ])
