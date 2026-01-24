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

module API
  ( app
  , createHandler
  , resolveHandler
  ) where

import Types
import Canonicalize (canonicalize)
import ShortCode (generateShortCode)
import Store
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.Wai
import Network.HTTP.Types
import Data.Aeson (object, (.=), encode)

-- | WAI Application
app :: Config -> SQLiteStore -> Application
app config store req respond =
  case (requestMethod req, pathInfo req) of
    ("POST", ["api", "create"]) -> createHandler config store req respond
    ("GET", [code]) -> resolveHandler store code req respond
    ("HEAD", [code]) -> resolveHandler store code req respond
    _ -> respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"

-- | Handle short link creation
createHandler :: Config -> SQLiteStore -> Application
createHandler config store req respond = do
  -- Parse request body
  body <- strictRequestBody req
  let params = parseQuery (BL.toStrict body)

  case lookup "url" params of
    Nothing -> respond $ errorResponse status400 "Missing 'url' parameter"
    Just Nothing -> respond $ errorResponse status400 "Empty 'url' parameter"
    Just (Just urlBytes) -> do
      let rawUrl = RawURL (TE.decodeUtf8 urlBytes)

      -- Canonicalize URL (IO operation via SPARK FFI)
      canonResult <- canonicalize rawUrl
      case canonResult of
        Left err -> respond $ errorResponse status400 (validationErrorMessage err)
        Right validUrl -> do
          -- Generate short code (IO operation via SPARK FFI)
          shortCode <- generateShortCode (cfgSecret config) validUrl

          -- Store mapping (idempotent)
          put shortCode validUrl store

          -- Return response
          let ShortCode code = shortCode
              response = object
                [ "short" .= ("http://localhost:8080/" <> code)
                , "code" .= code
                ]
          respond $ responseLBS status200 
            [("Content-Type", "application/json")]
            (encode response)

-- | Handle redirect resolution
resolveHandler :: SQLiteStore -> T.Text -> Application
resolveHandler store code _req respond = do
  -- Validate code format (8 base62 characters)
  if T.length code /= 8
    then respond $ errorResponse status404 "Not found"
    else do
      -- Lookup in store
      maybeUrl <- get (ShortCode code) store
      case maybeUrl of
        Nothing -> respond $ errorResponse status404 "Not found"
        Just (ValidURL url) ->
          -- Return 302 redirect
          respond $ responseLBS status302
            [ ("Location", TE.encodeUtf8 url)
            , ("Cache-Control", "public, max-age=3600")
            ]
            ""

-- | Helper to create error responses
errorResponse :: Status -> T.Text -> Response
errorResponse status msg =
  responseLBS status
    [("Content-Type", "application/json")]
    (encode $ object ["error" .= msg])

-- | Convert validation error to message
validationErrorMessage :: ValidationError -> T.Text
validationErrorMessage InvalidLength = "URL length invalid"
validationErrorMessage InvalidScheme = "Only http:// and https:// schemes allowed"
validationErrorMessage InvalidHost = "Invalid host"
validationErrorMessage PrivateAddress = "Private/local addresses not allowed"
validationErrorMessage CredentialsPresent = "URLs with credentials not allowed"
validationErrorMessage InvalidCharacters = "Invalid characters in URL"
validationErrorMessage (ParseError msg) = "Parse error: " <> msg
