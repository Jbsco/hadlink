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

