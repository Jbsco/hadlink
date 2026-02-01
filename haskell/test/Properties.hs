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

module Main (main) where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import Data.Either (isLeft, isRight)

import Types
import Canonicalize (canonicalize)
import ShortCode (generateShortCode)
import SparkFFI (initSpark, finalizeSpark)
import RateLimit (newRateLimiter, checkLimit)
import ProofOfWork (verifyPoW, leadingZeroBits)
import API (selectDifficulty)

import Control.Exception (bracket)
import Test.Tasty.Runners (NumThreads(..))

main :: IO ()
main = bracket initSpark (const finalizeSpark) $ \_ ->
  -- Run single-threaded: Ada FFI is not thread-safe for concurrent calls
  defaultMain $ localOption (NumThreads 1) tests

tests :: TestTree
tests = testGroup "hadlink Properties"
  [ canonicalizationTests
  , shortCodeTests
  , roundTripTests
  , negativeTests
  , rateLimitTests
  , proofOfWorkTests
  , effectiveDifficultyTests
  ]

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a valid public domain name
genPublicDomain :: Gen T.Text
genPublicDomain = do
  subdomain <- Gen.text (Range.linear 1 10) Gen.lower
  domain <- Gen.element ["example", "test", "hadlink", "mysite", "server"]
  tld <- Gen.element ["com", "org", "net", "io", "dev"]
  return $ subdomain <> "." <> domain <> "." <> tld

-- | Generate a valid path component
genPath :: Gen T.Text
genPath = do
  segments <- Gen.list (Range.linear 0 4) $
    Gen.text (Range.linear 1 20) (Gen.element pathChars)
  return $ T.intercalate "/" segments
  where
    pathChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']

-- | Generate a valid HTTP URL
genValidHttpURL :: Gen T.Text
genValidHttpURL = do
  scheme <- Gen.element ["http://", "https://"]
  domain <- genPublicDomain
  path <- genPath
  let pathPart = if T.null path then "" else "/" <> path
  return $ scheme <> domain <> pathPart

-- | Generate a private IP address
genPrivateIP :: Gen T.Text
genPrivateIP = Gen.choice
  [ do  -- 10.x.x.x
      a <- Gen.int (Range.linear 0 255)
      b <- Gen.int (Range.linear 0 255)
      c <- Gen.int (Range.linear 0 255)
      return $ "10." <> T.pack (show a) <> "." <> T.pack (show b) <> "." <> T.pack (show c)
  , do  -- 192.168.x.x
      a <- Gen.int (Range.linear 0 255)
      b <- Gen.int (Range.linear 0 255)
      return $ "192.168." <> T.pack (show a) <> "." <> T.pack (show b)
  , do  -- 172.16-31.x.x
      second <- Gen.int (Range.linear 16 31)
      a <- Gen.int (Range.linear 0 255)
      b <- Gen.int (Range.linear 0 255)
      return $ "172." <> T.pack (show second) <> "." <> T.pack (show a) <> "." <> T.pack (show b)
  , return "localhost"
  , return "127.0.0.1"
  ]

-- | Generate a URL with private IP
genPrivateURL :: Gen T.Text
genPrivateURL = do
  scheme <- Gen.element ["http://", "https://"]
  ip <- genPrivateIP
  path <- genPath
  let pathPart = if T.null path then "" else "/" <> path
  return $ scheme <> ip <> pathPart

-- | Generate a URL with credentials
genURLWithCredentials :: Gen T.Text
genURLWithCredentials = do
  scheme <- Gen.element ["http://", "https://"]
  user <- Gen.text (Range.linear 1 10) Gen.alphaNum
  pass <- Gen.text (Range.linear 1 10) Gen.alphaNum
  domain <- genPublicDomain
  return $ scheme <> user <> ":" <> pass <> "@" <> domain <> "/path"

-- | Generate a URL with invalid scheme
genInvalidSchemeURL :: Gen T.Text
genInvalidSchemeURL = do
  scheme <- Gen.element ["ftp://", "file://", "mailto:", "javascript:", "data:"]
  domain <- genPublicDomain
  return $ scheme <> domain

-- | Generate a 32-byte secret key
genSecret :: Gen BS.ByteString
genSecret = BS8.pack <$> Gen.list (Range.singleton 32) Gen.alphaNum

--------------------------------------------------------------------------------
-- Canonicalization Tests
--------------------------------------------------------------------------------

canonicalizationTests :: TestTree
canonicalizationTests = testGroup "URL Canonicalization"
  [ testProperty "valid HTTP URLs pass" prop_valid_http_url
  , testProperty "valid HTTPS URLs pass" prop_valid_https_url
  , testProperty "canonicalization preserves URL" prop_preserves_url
  ]

-- | Valid HTTP URLs should pass canonicalization
prop_valid_http_url :: Property
prop_valid_http_url = property $ do
  url <- forAll $ do
    domain <- genPublicDomain
    path <- genPath
    let pathPart = if T.null path then "" else "/" <> path
    return $ "http://" <> domain <> pathPart
  result <- evalIO $ canonicalize (RawURL url)
  assert $ isRight result

-- | Valid HTTPS URLs should pass canonicalization
prop_valid_https_url :: Property
prop_valid_https_url = property $ do
  url <- forAll $ do
    domain <- genPublicDomain
    path <- genPath
    let pathPart = if T.null path then "" else "/" <> path
    return $ "https://" <> domain <> pathPart
  result <- evalIO $ canonicalize (RawURL url)
  assert $ isRight result

-- | Canonicalization should preserve the URL content
prop_preserves_url :: Property
prop_preserves_url = property $ do
  url <- forAll genValidHttpURL
  result <- evalIO $ canonicalize (RawURL url)
  case result of
    Left _ -> success  -- If rejected, that's fine (could be edge case)
    Right (ValidURL canonical) -> canonical === url

--------------------------------------------------------------------------------
-- Short Code Tests
--------------------------------------------------------------------------------

shortCodeTests :: TestTree
shortCodeTests = testGroup "Short Code Generation"
  [ testProperty "deterministic: same input = same output" prop_deterministic
  , testProperty "fixed length (8 characters)" prop_fixed_length
  , testProperty "base62 alphabet only" prop_base62_alphabet
  , testProperty "different URLs produce different codes" prop_different_urls
  ]

-- | Same URL and secret should always produce the same short code
prop_deterministic :: Property
prop_deterministic = property $ do
  url <- forAll genValidHttpURL
  secret <- forAll genSecret

  -- Canonicalize first
  canonResult <- evalIO $ canonicalize (RawURL url)
  case canonResult of
    Left _ -> success  -- Skip if URL rejected
    Right validUrl -> do
      -- Generate short code twice
      code1 <- evalIO $ generateShortCode secret validUrl
      code2 <- evalIO $ generateShortCode secret validUrl
      code1 === code2

-- | Short codes should always be exactly 8 characters
prop_fixed_length :: Property
prop_fixed_length = property $ do
  url <- forAll genValidHttpURL
  secret <- forAll genSecret

  canonResult <- evalIO $ canonicalize (RawURL url)
  case canonResult of
    Left _ -> success
    Right validUrl -> do
      ShortCode code <- evalIO $ generateShortCode secret validUrl
      T.length code === 8

-- | Short codes should only use base62 characters
prop_base62_alphabet :: Property
prop_base62_alphabet = property $ do
  url <- forAll genValidHttpURL
  secret <- forAll genSecret

  canonResult <- evalIO $ canonicalize (RawURL url)
  case canonResult of
    Left _ -> success
    Right validUrl -> do
      ShortCode code <- evalIO $ generateShortCode secret validUrl
      assert $ T.all isBase62 code
  where
    isBase62 c = isDigit c || isAsciiLower c || isAsciiUpper c

-- | Different URLs should (almost always) produce different short codes
prop_different_urls :: Property
prop_different_urls = property $ do
  url1 <- forAll genValidHttpURL
  url2 <- forAll $ Gen.filter (/= url1) genValidHttpURL
  secret <- forAll genSecret

  canonResult1 <- evalIO $ canonicalize (RawURL url1)
  canonResult2 <- evalIO $ canonicalize (RawURL url2)

  case (canonResult1, canonResult2) of
    (Right validUrl1, Right validUrl2) -> do
      code1 <- evalIO $ generateShortCode secret validUrl1
      code2 <- evalIO $ generateShortCode secret validUrl2
      -- Different URLs should produce different codes
      assert $ code1 /= code2
    _ -> success  -- Skip if either URL rejected

--------------------------------------------------------------------------------
-- Round-Trip Tests
--------------------------------------------------------------------------------

roundTripTests :: TestTree
roundTripTests = testGroup "Round-Trip"
  [ testProperty "canonicalize is idempotent" prop_canonicalize_idempotent
  , testProperty "ValidURL can be re-canonicalized" prop_recanonicalizable
  ]

-- | Canonicalizing a valid URL twice should give the same result
prop_canonicalize_idempotent :: Property
prop_canonicalize_idempotent = property $ do
  url <- forAll genValidHttpURL

  result1 <- evalIO $ canonicalize (RawURL url)
  case result1 of
    Left _ -> success
    Right (ValidURL canonical) -> do
      result2 <- evalIO $ canonicalize (RawURL canonical)
      case result2 of
        Left _ -> failure  -- Should not fail on already-valid URL
        Right (ValidURL canonical2) -> canonical === canonical2

-- | A ValidURL should successfully re-canonicalize to itself
prop_recanonicalizable :: Property
prop_recanonicalizable = property $ do
  url <- forAll genValidHttpURL

  result <- evalIO $ canonicalize (RawURL url)
  case result of
    Left _ -> success
    Right validUrl@(ValidURL canonical) -> do
      result2 <- evalIO $ canonicalize (RawURL canonical)
      result2 === Right validUrl

--------------------------------------------------------------------------------
-- Negative Tests
--------------------------------------------------------------------------------

negativeTests :: TestTree
negativeTests = testGroup "Negative Tests (Rejection)"
  [ testProperty "private IPs rejected" prop_reject_private_ip
  , testProperty "credentials rejected" prop_reject_credentials
  , testProperty "invalid schemes rejected" prop_reject_invalid_scheme
  , testProperty "empty URL rejected" prop_reject_empty
  , testProperty "too long URL rejected" prop_reject_too_long
  ]

-- | URLs with private IPs should be rejected
prop_reject_private_ip :: Property
prop_reject_private_ip = property $ do
  url <- forAll genPrivateURL
  result <- evalIO $ canonicalize (RawURL url)
  assert $ isLeft result

-- | URLs with credentials should be rejected
prop_reject_credentials :: Property
prop_reject_credentials = property $ do
  url <- forAll genURLWithCredentials
  result <- evalIO $ canonicalize (RawURL url)
  assert $ isLeft result

-- | URLs with non-HTTP(S) schemes should be rejected
prop_reject_invalid_scheme :: Property
prop_reject_invalid_scheme = property $ do
  url <- forAll genInvalidSchemeURL
  result <- evalIO $ canonicalize (RawURL url)
  assert $ isLeft result

-- | Empty URLs should be rejected
prop_reject_empty :: Property
prop_reject_empty = property $ do
  result <- evalIO $ canonicalize (RawURL "")
  assert $ isLeft result

-- | URLs over 2048 characters should be rejected
prop_reject_too_long :: Property
prop_reject_too_long = property $ do
  -- Generate a URL that's definitely too long
  longPath <- forAll $ Gen.text (Range.singleton 2100) Gen.alphaNum
  let url = "https://example.com/" <> longPath
  result <- evalIO $ canonicalize (RawURL url)
  assert $ isLeft result

--------------------------------------------------------------------------------
-- Rate Limiting Tests
--------------------------------------------------------------------------------

rateLimitTests :: TestTree
rateLimitTests = testGroup "Rate Limiting"
  [ testProperty "requests within limit allowed" prop_within_limit
  , testProperty "requests exceeding limit rejected" prop_exceeds_limit
  , testProperty "different clients have separate limits" prop_separate_clients
  ]

-- | Generate a client IP
genClientIP :: Gen ClientIP
genClientIP = do
  a <- Gen.int (Range.linear 1 254)
  b <- Gen.int (Range.linear 0 255)
  c <- Gen.int (Range.linear 0 255)
  d <- Gen.int (Range.linear 1 254)
  let ip = BS8.pack $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
  return $ ClientIP ip

-- | Create a test config with specified rate limit settings
testConfig :: Int -> Int -> Config
testConfig limit window = Config
  { cfgSecret = "testsecrettestsecrettestsecrett"  -- 32 bytes
  , cfgPowDifficulty = Difficulty 0
  , cfgPowDifficultyAuth = Difficulty 0
  , cfgRateLimitPerIP = limit
  , cfgRateLimitWindow = window
  , cfgStoragePath = ":memory:"
  , cfgAPIKeys = []
  , cfgTrustProxy = False
  , cfgBaseURL = "http://localhost:8443"
  }

-- | Requests within the rate limit should be allowed
prop_within_limit :: Property
prop_within_limit = property $ do
  client <- forAll genClientIP
  -- Create limiter with 10 tokens
  limiter <- evalIO $ newRateLimiter (testConfig 10 60)

  -- First request should always be allowed
  result <- evalIO $ checkLimit limiter client
  assert result

-- | Requests exceeding the rate limit should be rejected
prop_exceeds_limit :: Property
prop_exceeds_limit = property $ do
  client <- forAll genClientIP
  -- Create limiter with only 2 tokens
  limiter <- evalIO $ newRateLimiter (testConfig 2 60)

  -- Exhaust the bucket
  _ <- evalIO $ checkLimit limiter client  -- 1st: allowed
  _ <- evalIO $ checkLimit limiter client  -- 2nd: allowed
  result <- evalIO $ checkLimit limiter client  -- 3rd: should be rejected
  assert $ not result

-- | Different clients should have separate rate limit buckets
prop_separate_clients :: Property
prop_separate_clients = property $ do
  client1 <- forAll genClientIP
  client2 <- forAll $ Gen.filter (/= client1) genClientIP
  -- Create limiter with only 1 token
  limiter <- evalIO $ newRateLimiter (testConfig 1 60)

  -- Exhaust client1's bucket
  _ <- evalIO $ checkLimit limiter client1
  result1 <- evalIO $ checkLimit limiter client1  -- client1 exhausted

  -- client2 should still have tokens
  result2 <- evalIO $ checkLimit limiter client2  -- client2 has tokens

  assert $ not result1  -- client1 rejected
  assert result2        -- client2 allowed

--------------------------------------------------------------------------------
-- Proof of Work Tests
--------------------------------------------------------------------------------

proofOfWorkTests :: TestTree
proofOfWorkTests = testGroup "Proof of Work"
  [ testProperty "leadingZeroBits counts correctly" prop_leading_zero_bits
  , testProperty "difficulty 0 always passes" prop_pow_disabled
  , testProperty "verification is deterministic" prop_pow_deterministic
  , testProperty "valid proof passes" prop_pow_valid_proof
  ]

-- | Generate a random nonce
genNonce :: Gen Nonce
genNonce = Nonce <$> Gen.bytes (Range.linear 8 32)

-- | leadingZeroBits should correctly count leading zeros
prop_leading_zero_bits :: Property
prop_leading_zero_bits = property $ do
  -- Test known values
  -- 0x00 = 8 leading zeros
  assert $ leadingZeroBits (BS.pack [0x00]) == 8
  -- 0x00 0x00 = 16 leading zeros
  assert $ leadingZeroBits (BS.pack [0x00, 0x00]) == 16
  -- 0x80 = 0 leading zeros (bit 7 is set)
  assert $ leadingZeroBits (BS.pack [0x80]) == 0
  -- 0x40 = 1 leading zero (bit 6 is set)
  assert $ leadingZeroBits (BS.pack [0x40]) == 1
  -- 0x01 = 7 leading zeros
  assert $ leadingZeroBits (BS.pack [0x01]) == 7
  -- 0x00 0x80 = 8 leading zeros (first byte all zeros, second has bit 7 set)
  assert $ leadingZeroBits (BS.pack [0x00, 0x80]) == 8
  -- Empty bytestring = 0 leading zeros
  assert $ leadingZeroBits BS.empty == 0

-- | When difficulty is 0, PoW is disabled and always passes
prop_pow_disabled :: Property
prop_pow_disabled = property $ do
  url <- forAll genValidHttpURL
  nonce <- forAll genNonce

  canonResult <- evalIO $ canonicalize (RawURL url)
  case canonResult of
    Left _ -> success
    Right validUrl -> do
      -- Difficulty 0 should always pass regardless of nonce
      let result = verifyPoW (Difficulty 0) validUrl nonce
      assert result

-- | PoW verification should be deterministic
prop_pow_deterministic :: Property
prop_pow_deterministic = property $ do
  url <- forAll genValidHttpURL
  nonce <- forAll genNonce
  powDiff <- forAll $ Difficulty <$> Gen.int (Range.linear 0 8)

  canonResult <- evalIO $ canonicalize (RawURL url)
  case canonResult of
    Left _ -> success
    Right validUrl -> do
      -- Same inputs should produce same result
      let result1 = verifyPoW powDiff validUrl nonce
          result2 = verifyPoW powDiff validUrl nonce
      result1 === result2

-- | A valid proof should pass verification
-- We test by finding a nonce that satisfies low difficulty (1-4 bits)
prop_pow_valid_proof :: Property
prop_pow_valid_proof = property $ do
  url <- forAll genValidHttpURL
  -- Use low difficulty to make finding valid nonce feasible
  powDiff <- forAll $ Difficulty <$> Gen.int (Range.linear 1 4)

  canonResult <- evalIO $ canonicalize (RawURL url)
  case canonResult of
    Left _ -> success
    Right validUrl -> do
      -- Try random nonces until we find one that works (or give up)
      nonces <- forAll $ Gen.list (Range.singleton 100) genNonce
      let validNonces = filter (verifyPoW powDiff validUrl) nonces
      -- With difficulty 1-4 and 100 attempts, we should find at least one
      -- Probability of all 100 failing at difficulty 4 is (15/16)^100 ≈ 0.001
      -- At difficulty 1, it's (1/2)^100 ≈ 0
      assert $ not (null validNonces)

--------------------------------------------------------------------------------
-- Effective Difficulty Tests
--------------------------------------------------------------------------------

effectiveDifficultyTests :: TestTree
effectiveDifficultyTests = testGroup "Effective Difficulty Selection"
  [ testProperty "anonymous requests use cfgPowDifficulty" prop_anon_uses_pow_difficulty
  , testProperty "valid API key uses cfgPowDifficultyAuth" prop_auth_uses_auth_difficulty
  , testProperty "invalid API key uses cfgPowDifficulty" prop_invalid_key_uses_anon_difficulty
  ]

-- | Create a test config with specified PoW difficulties
testPowConfig :: Int -> Int -> [T.Text] -> Config
testPowConfig anonDiff authDiff keys = Config
  { cfgSecret = "testsecrettestsecrettestsecrett"  -- 32 bytes
  , cfgPowDifficulty = Difficulty anonDiff
  , cfgPowDifficultyAuth = Difficulty authDiff
  , cfgRateLimitPerIP = 10
  , cfgRateLimitWindow = 60
  , cfgStoragePath = ":memory:"
  , cfgAPIKeys = map APIKey keys
  , cfgTrustProxy = False
  , cfgBaseURL = "http://localhost:8443"
  }

-- | Anonymous requests (no API key) should use cfgPowDifficulty
prop_anon_uses_pow_difficulty :: Property
prop_anon_uses_pow_difficulty = property $ do
  anonDiff <- forAll $ Gen.int (Range.linear 0 16)
  authDiff <- forAll $ Gen.int (Range.linear 0 16)

  let config = testPowConfig anonDiff authDiff ["valid-key"]
      result = selectDifficulty config Nothing

  result === anonDiff

-- | Requests with valid API key should use cfgPowDifficultyAuth
prop_auth_uses_auth_difficulty :: Property
prop_auth_uses_auth_difficulty = property $ do
  anonDiff <- forAll $ Gen.int (Range.linear 0 16)
  authDiff <- forAll $ Gen.int (Range.linear 0 16)

  let config = testPowConfig anonDiff authDiff ["valid-key", "another-key"]
      result = selectDifficulty config (Just (APIKey "valid-key"))

  result === authDiff

-- | Requests with invalid API key should use cfgPowDifficulty (treated as anonymous)
prop_invalid_key_uses_anon_difficulty :: Property
prop_invalid_key_uses_anon_difficulty = property $ do
  anonDiff <- forAll $ Gen.int (Range.linear 0 16)
  authDiff <- forAll $ Gen.int (Range.linear 0 16)

  let config = testPowConfig anonDiff authDiff ["valid-key"]
      result = selectDifficulty config (Just (APIKey "wrong-key"))

  result === anonDiff
