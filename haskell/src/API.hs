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
import Network.HTTP.Types.URI (parseQuery)
import Data.Aeson (object, (.=), encode)

-- | WAI Application
app :: Config -> SQLiteStore -> Application
app config store req respond =
  case (requestMethod req, pathInfo req) of
    ("POST", ["api", "create"]) -> createHandler config store req respond
    ("GET", [code]) -> resolveHandler store code req respond
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

      -- Canonicalize URL
      case canonicalize rawUrl of
        Left err -> respond $ errorResponse status400 (validationErrorMessage err)
        Right validUrl -> do
          -- Generate short code
          let shortCode = generateShortCode (cfgSecret config) validUrl

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
