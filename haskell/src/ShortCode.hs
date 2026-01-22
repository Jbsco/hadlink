{-# LANGUAGE OverloadedStrings #-}

module ShortCode
  ( generateShortCode
  , base62Encode
  , base62Alphabet
  ) where

import Types
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Crypto.MAC.HMAC (hmac, HMAC)
import Crypto.Hash (SHA256)
import Data.ByteArray (convert)

-- | Generate a deterministic 8-character short code
-- Phase 1: Pure Haskell implementation using HMAC-SHA256 + Base62
-- Phase 2: This will be replaced with FFI call to SPARK
generateShortCode :: BS.ByteString -> ValidURL -> ShortCode
generateShortCode secret (ValidURL url) =
  let urlBytes = encodeUtf8 url
      -- HMAC-SHA256(secret, url)
      mac :: HMAC SHA256
      mac = hmac secret urlBytes
      digest = convert mac :: BS.ByteString
      -- Take first 8 bytes and encode to base62
      shortBytes = BS.take 6 digest  -- 6 bytes gives us good entropy
      encoded = base62Encode shortBytes
  in ShortCode (T.take 8 encoded)

-- | Base62 alphabet (0-9, a-z, A-Z)
base62Alphabet :: String
base62Alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Encode bytes to base62
-- Takes bytes and produces a base62 string
base62Encode :: BS.ByteString -> T.Text
base62Encode bs
  | BS.null bs = "0"
  | otherwise = T.pack $ reverse $ go (bytesToInteger bs) []
  where
    go 0 acc = if null acc then "0" else acc
    go n acc = 
      let (q, r) = n `divMod` 62
          c = base62Alphabet !! fromIntegral r
      in go q (c : acc)

    -- Convert bytes to a big integer
    bytesToInteger :: BS.ByteString -> Integer
    bytesToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

