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
  , selectDifficulty
  ) where

import Types
import Canonicalize (canonicalize)
import ShortCode (generateShortCode)
import Store
import RateLimit (RateLimiter, checkLimit)
import ProofOfWork (verifyPoW)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.Wai
import Network.HTTP.Types
import Network.Socket (SockAddr(..))
import Data.Aeson (object, (.=), encode)
import Data.Word (Word8, Word32)

-- | WAI Application
app :: Config -> SQLiteStore -> RateLimiter -> Application
app config store limiter req respond =
  case (requestMethod req, pathInfo req) of
    ("POST", ["api", "create"]) -> createHandler config store limiter req respond
    ("GET", [code]) -> resolveHandler store code req respond
    ("HEAD", [code]) -> resolveHandler store code req respond
    _ -> respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"

-- | Handle short link creation
createHandler :: Config -> SQLiteStore -> RateLimiter -> Application
createHandler config store limiter req respond = do
  -- Extract client IP and check rate limit
  let clientIP = getClientIP config req
  allowed <- checkLimit limiter clientIP
  if not allowed
    then respond $ errorResponse status429 "Rate limit exceeded"
    else handleCreate config store req respond

-- | Actual create logic (after rate limit check)
handleCreate :: Config -> SQLiteStore -> Application
handleCreate config store req respond = do
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
          -- Determine PoW difficulty based on authentication status
          let effectiveDifficulty = getEffectiveDifficulty config req
          if effectiveDifficulty == 0
            then
              -- PoW disabled for this request
              createShortLink config store validUrl respond
            else
              -- PoW required - verify nonce
              case lookup "nonce" params of
                Nothing -> respond $ errorResponse status400 "Missing 'nonce' parameter (proof-of-work required)"
                Just Nothing -> respond $ errorResponse status400 "Empty 'nonce' parameter"
                Just (Just nonceBytes) -> do
                  let nonce = Nonce nonceBytes
                  if verifyPoW (Difficulty effectiveDifficulty) validUrl nonce
                    then createShortLink config store validUrl respond
                    else respond $ errorResponse status400 "Proof-of-work verification failed"

-- | Get effective PoW difficulty for this request
-- Authenticated requests use cfgPowDifficultyAuth, anonymous use cfgPowDifficulty
getEffectiveDifficulty :: Config -> Request -> Int
getEffectiveDifficulty config req =
  let apiKeyHeader = lookup "X-API-Key" (requestHeaders req)
      maybeKey = fmap (APIKey . TE.decodeUtf8) apiKeyHeader
  in selectDifficulty config maybeKey

-- | Pure function to select difficulty based on API key
-- Exported for testing without wai dependency
selectDifficulty :: Config -> Maybe APIKey -> Int
selectDifficulty config maybeKey =
  let Difficulty anonDiff = cfgPowDifficulty config
      Difficulty authDiff = cfgPowDifficultyAuth config
      hasValidKey = case maybeKey of
        Nothing -> False
        Just key -> key `elem` cfgAPIKeys config
  in if hasValidKey then authDiff else anonDiff

-- | Create short link and respond (shared by PoW and non-PoW paths)
createShortLink :: Config -> SQLiteStore -> ValidURL -> (Response -> IO ResponseReceived) -> IO ResponseReceived
createShortLink config store validUrl respond = do
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

-- | Extract client IP from request
-- Only checks X-Forwarded-For when cfgTrustProxy is True (behind trusted reverse proxy)
-- When disabled, always uses the direct socket address to prevent header spoofing attacks
getClientIP :: Config -> Request -> ClientIP
getClientIP config req
  | cfgTrustProxy config =
      -- Trust proxy mode: use X-Forwarded-For if present
      case lookup "X-Forwarded-For" (requestHeaders req) of
        Just xff ->
          -- X-Forwarded-For may contain multiple IPs; take the first (original client)
          -- Validate it looks like an IP address (basic check for alphanumeric, dots, colons)
          let firstIP = BS.takeWhile (/= 44) xff  -- 44 = comma
              isValidIPChar c = (c >= 48 && c <= 57)   -- 0-9
                             || (c >= 65 && c <= 70)   -- A-F (for IPv6)
                             || (c >= 97 && c <= 102)  -- a-f (for IPv6)
                             || c == 46                -- .
                             || c == 58                -- : (for IPv6)
          in if BS.all isValidIPChar firstIP && not (BS.null firstIP)
             then ClientIP firstIP
             else ClientIP $ sockAddrToBS (remoteHost req)  -- Invalid format, use socket
        Nothing ->
          ClientIP $ sockAddrToBS (remoteHost req)
  | otherwise =
      -- Direct mode: always use socket address (prevents header spoofing)
      ClientIP $ sockAddrToBS (remoteHost req)

-- | Convert socket address to ByteString for use as client identifier
sockAddrToBS :: SockAddr -> BS.ByteString
sockAddrToBS (SockAddrInet _port host) =
  -- IPv4: convert host address to string representation
  let a = fromIntegral (host `mod` 256)
      b = fromIntegral ((host `div` 256) `mod` 256)
      c = fromIntegral ((host `div` 65536) `mod` 256)
      d = fromIntegral (host `div` 16777216)
  in BS.pack [a, 46, b, 46, c, 46, d]  -- "a.b.c.d" as bytes
sockAddrToBS (SockAddrInet6 _port _flow host _scope) =
  -- IPv6: use raw bytes as identifier
  let (a, b, c, d) = host
  in BS.pack $ concatMap word32ToBytes [a, b, c, d]
sockAddrToBS (SockAddrUnix path) =
  -- Unix socket: use path as identifier
  TE.encodeUtf8 (T.pack path)

-- | Convert Word32 to list of bytes
word32ToBytes :: Word32 -> [Word8]
word32ToBytes w =
  [ fromIntegral (w `mod` 256)
  , fromIntegral ((w `div` 256) `mod` 256)
  , fromIntegral ((w `div` 65536) `mod` 256)
  , fromIntegral (w `div` 16777216)
  ]

