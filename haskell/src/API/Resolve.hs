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

module API.Resolve
  ( resolveApp
  , resolveHandler
  ) where

import Types
import Store
import Logging (Logger, logRequest)
import Health (healthHandler)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai
import Network.HTTP.Types


-- | WAI Application for the redirect/resolve service
resolveApp :: SQLiteStore -> Logger -> Application
resolveApp store logger req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", ["health"]) -> healthHandler store req respond
    ("GET", [code]) -> resolveHandler store logger code req respond
    ("HEAD", [code]) -> resolveHandler store logger code req respond
    _ -> do
      let resp = responseLBS status404
            [("Content-Type", "text/plain")] "Not found"
      logRequest logger req status404
      respond resp

-- | Handle redirect resolution
resolveHandler :: SQLiteStore -> Logger -> T.Text -> Application
resolveHandler store logger code _req respond = do
  if T.length code /= 8
    then do
      let resp = responseLBS status404
            [("Content-Type", "text/plain")] "Not found"
      logRequest logger _req status404
      respond resp
    else do
      maybeUrl <- get (ShortCode code) store
      case maybeUrl of
        Nothing -> do
          let resp = responseLBS status404
                [("Content-Type", "text/plain")] "Not found"
          logRequest logger _req status404
          respond resp
        Just (ValidURL url) -> do
          let resp = responseLBS status302
                [ ("Location", TE.encodeUtf8 url)
                , ("Cache-Control", "public, max-age=3600")
                ]
                ""
          logRequest logger _req status302
          respond resp
