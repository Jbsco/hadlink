{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

-- | Raw, unvalidated URL from user input
newtype RawURL = RawURL Text
  deriving (Show, Eq, Generic)

-- | Validated and canonicalized URL (can only be constructed via SPARK validation)
newtype ValidURL = ValidURL Text
  deriving (Show, Eq, Generic)

-- | Short code (deterministic, non-enumerable)
newtype ShortCode = ShortCode Text
  deriving (Show, Eq, Ord, Generic)

-- | Client IP address for rate limiting
newtype ClientIP = ClientIP ByteString
  deriving (Show, Eq, Ord, Generic)

-- | Proof-of-Work difficulty (number of leading zero bits required)
newtype Difficulty = Difficulty Int
  deriving (Show, Eq, Ord, Num, Generic)

-- | API key for authenticated creation (bypasses PoW)
newtype APIKey = APIKey Text
  deriving (Show, Eq, Generic)

-- | Nonce for proof-of-work
newtype Nonce = Nonce ByteString
  deriving (Show, Eq, Generic)

-- | Error types for URL validation
data ValidationError
  = InvalidLength
  | InvalidScheme
  | InvalidHost
  | PrivateAddress
  | CredentialsPresent
  | InvalidCharacters
  | ParseError Text
  deriving (Show, Eq, Generic)

-- | Error types for API operations
data APIError
  = ValidationFailed ValidationError
  | RateLimitExceeded
  | ProofOfWorkFailed
  | StorageError Text
  | InternalError Text
  deriving (Show, Eq, Generic)

-- | Configuration for the shortener service
data Config = Config
  { cfgSecret          :: ByteString  -- HMAC secret for short codes
  , cfgPowDifficulty   :: Difficulty  -- PoW difficulty (0 = disabled)
  , cfgRateLimitPerIP  :: Int         -- Max requests per IP per window
  , cfgRateLimitWindow :: Int         -- Rate limit window in seconds
  , cfgStoragePath     :: FilePath    -- Path to storage backend
  , cfgAPIKeys         :: [APIKey]    -- Authorized API keys (bypass PoW)
  } deriving (Show, Eq, Generic)
