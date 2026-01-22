{-# LANGUAGE OverloadedStrings #-}

module ProofOfWork
  ( verifyPoW
  , leadingZeroBits
  ) where

import Types
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash (hash, Digest, SHA256)
import Data.ByteArray (convert)
import Data.Bits (testBit)
import Data.Word (Word8)

-- | Verify proof-of-work for a URL
-- Client must provide nonce such that SHA256(url || nonce) has N leading zero bits
verifyPoW :: Difficulty -> ValidURL -> Nonce -> Bool
verifyPoW (Difficulty diff) (ValidURL url) (Nonce nonce)
  | diff == 0 = True  -- PoW disabled
  | otherwise =
      let urlBytes = encodeUtf8 url
          combined = BS.append urlBytes nonce
          digest = hash combined :: Digest SHA256
          digestBytes = convert digest :: BS.ByteString
      in leadingZeroBits digestBytes >= diff

-- | Count leading zero bits in a bytestring
leadingZeroBits :: BS.ByteString -> Int
leadingZeroBits bs = go 0 (BS.unpack bs)
  where
    go count [] = count
    go count (byte:rest)
      | byte == 0 = go (count + 8) rest
      | otherwise = count + countLeadingZeros byte

    -- Count leading zeros in a single byte
    countLeadingZeros :: Word8 -> Int
    countLeadingZeros byte = length $ takeWhile not [testBit byte i | i <- [7,6..0]]
