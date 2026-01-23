{-# LANGUAGE OverloadedStrings #-}

module ShortCode
  ( generateShortCode
  , base62Encode
  , base62Alphabet
  ) where

import Types
import SparkFFI (sparkMakeShortCode)
import qualified Data.Text as T
import qualified Data.ByteString as BS

-- | Generate a deterministic 8-character short code
-- Phase 2.5: Uses SPARK core via FFI for short code generation
generateShortCode :: BS.ByteString -> ValidURL -> IO ShortCode
generateShortCode secret (ValidURL url) = do
  code <- sparkMakeShortCode secret url
  return $ ShortCode code

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

