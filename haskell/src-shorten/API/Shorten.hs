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

module API.Shorten
  ( shortenApp
  , createHandler
  , handleCreate
  , createShortLink
  , getEffectiveDifficulty
  , selectDifficulty
  , errorResponse
  , validationErrorMessage
  , getClientIP
  , sockAddrToBS
  , word32ToBytes
  ) where

import Types
import Canonicalize (canonicalize)
import ShortCode (generateShortCode)
import Store
import RateLimit (RateLimiter, checkLimit)
import ProofOfWork (verifyPoW)
import API.Resolve (resolveHandler)
import Logging (Logger, logRequest, logInfo, logWarn)
import Health (healthHandler)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.Wai
import Network.HTTP.Types
import Network.Socket (SockAddr(..))
import Data.Aeson (object, (.=), encode)
import Data.Word (Word8, Word32)

-- | WAI Application for the shorten service (includes resolve)
shortenApp :: Config -> SQLiteStore -> RateLimiter -> Logger -> Application
shortenApp config store limiter logger req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", ["health"]) -> healthHandler store req respond
    ("POST", ["api", "create"]) ->
      createHandler config store limiter logger req respond
    ("GET", [code]) -> resolveHandler store logger code req respond
    ("HEAD", [code]) -> resolveHandler store logger code req respond
    _ -> do
      logRequest logger req status404
      respond $ responseLBS status404
        [("Content-Type", "text/plain")] "Not found"

-- | Handle short link creation
createHandler :: Config -> SQLiteStore -> RateLimiter -> Logger -> Application
createHandler config store limiter logger req respond = do
  let clientIP = getClientIP config req
  allowed <- checkLimit limiter clientIP
  if not allowed
    then do
      logWarn logger "Rate limit exceeded" [("client_ip", T.pack $ show clientIP)]
      logRequest logger req status429
      respond $ errorResponse status429 "Rate limit exceeded"
    else handleCreate config store logger req respond

-- | Actual create logic (after rate limit check)
handleCreate :: Config -> SQLiteStore -> Logger -> Application
handleCreate config store logger req respond = do
  body <- strictRequestBody req
  let params = parseQuery (BL.toStrict body)

  case lookup "url" params of
    Nothing -> do
      logRequest logger req status400
      respond $ errorResponse status400 "Missing 'url' parameter"
    Just Nothing -> do
      logRequest logger req status400
      respond $ errorResponse status400 "Empty 'url' parameter"
    Just (Just urlBytes) -> do
      let rawUrl = RawURL (TE.decodeUtf8 urlBytes)

      canonResult <- canonicalize rawUrl
      case canonResult of
        Left err -> do
          logRequest logger req status400
          respond $ errorResponse status400
            (validationErrorMessage err)
        Right validUrl -> do
          let effectiveDifficulty = getEffectiveDifficulty config req
          if effectiveDifficulty == 0
            then
              createShortLink config store logger req validUrl respond
            else
              case lookup "nonce" params of
                Nothing -> do
                  logRequest logger req status400
                  respond $ errorResponse status400
                    "Missing 'nonce' parameter (proof-of-work required)"
                Just Nothing -> do
                  logRequest logger req status400
                  respond $ errorResponse status400
                    "Empty 'nonce' parameter"
                Just (Just nonceBytes) -> do
                  let nonce = Nonce nonceBytes
                  if verifyPoW (Difficulty effectiveDifficulty) validUrl nonce
                    then createShortLink config store logger req validUrl respond
                    else do
                      logRequest logger req status400
                      respond $ errorResponse status400
                        "Proof-of-work verification failed"

-- | Get effective PoW difficulty for this request
getEffectiveDifficulty :: Config -> Request -> Int
getEffectiveDifficulty config req =
  let apiKeyHeader = lookup "X-API-Key" (requestHeaders req)
      maybeKey = fmap (APIKey . TE.decodeUtf8) apiKeyHeader
  in selectDifficulty config maybeKey

-- | Pure function to select difficulty based on API key
selectDifficulty :: Config -> Maybe APIKey -> Int
selectDifficulty config maybeKey =
  let Difficulty anonDiff = cfgPowDifficulty config
      Difficulty authDiff = cfgPowDifficultyAuth config
      hasValidKey = case maybeKey of
        Nothing -> False
        Just key -> key `elem` cfgAPIKeys config
  in if hasValidKey then authDiff else anonDiff

-- | Create short link and respond
createShortLink :: Config -> SQLiteStore -> Logger -> Request -> ValidURL -> (Response -> IO ResponseReceived) -> IO ResponseReceived
createShortLink config store logger req validUrl respond = do
  shortCode <- generateShortCode (cfgSecret config) validUrl
  put shortCode validUrl store

  let ShortCode code = shortCode
      response = object
        [ "short" .= ("http://localhost:8080/" <> code)
        , "code" .= code
        ]
  logInfo logger "Link created" [("code", code)]
  logRequest logger req status200
  respond $ responseLBS status200
    [("Content-Type", "application/json")]
    (encode response)

-- | Helper to create error responses
errorResponse :: Status -> T.Text -> Response
errorResponse status msg =
  responseLBS status
    [("Content-Type", "application/json")]
    (encode $ object ["error" .= msg])

-- | Convert validation error to message
validationErrorMessage :: ValidationError -> T.Text
validationErrorMessage InvalidLength = "URL length invalid"
validationErrorMessage InvalidScheme =
  "Only http:// and https:// schemes allowed"
validationErrorMessage InvalidHost = "Invalid host"
validationErrorMessage PrivateAddress =
  "Private/local addresses not allowed"
validationErrorMessage CredentialsPresent =
  "URLs with credentials not allowed"
validationErrorMessage InvalidCharacters =
  "Invalid characters in URL"
validationErrorMessage (ParseError msg) = "Parse error: " <> msg

-- | Extract client IP from request
getClientIP :: Config -> Request -> ClientIP
getClientIP config req
  | cfgTrustProxy config =
      case lookup "X-Forwarded-For" (requestHeaders req) of
        Just xff ->
          let firstIP = BS.takeWhile (/= 44) xff
              isValidIPChar c = (c >= 48 && c <= 57)
                             || (c >= 65 && c <= 70)
                             || (c >= 97 && c <= 102)
                             || c == 46
                             || c == 58
          in if BS.all isValidIPChar firstIP && not (BS.null firstIP)
             then ClientIP firstIP
             else ClientIP $ sockAddrToBS (remoteHost req)
        Nothing ->
          ClientIP $ sockAddrToBS (remoteHost req)
  | otherwise =
      ClientIP $ sockAddrToBS (remoteHost req)

-- | Convert socket address to ByteString
sockAddrToBS :: SockAddr -> BS.ByteString
sockAddrToBS (SockAddrInet _port host) =
  let a = fromIntegral (host `mod` 256)
      b = fromIntegral ((host `div` 256) `mod` 256)
      c = fromIntegral ((host `div` 65536) `mod` 256)
      d = fromIntegral (host `div` 16777216)
  in BS.pack [a, 46, b, 46, c, 46, d]
sockAddrToBS (SockAddrInet6 _port _flow host _scope) =
  let (a, b, c, d) = host
  in BS.pack $ concatMap word32ToBytes [a, b, c, d]
sockAddrToBS (SockAddrUnix path) =
  TE.encodeUtf8 (T.pack path)

-- | Convert Word32 to list of bytes
word32ToBytes :: Word32 -> [Word8]
word32ToBytes w =
  [ fromIntegral (w `mod` 256)
  , fromIntegral ((w `div` 256) `mod` 256)
  , fromIntegral ((w `div` 65536) `mod` 256)
  , fromIntegral (w `div` 16777216)
  ]
