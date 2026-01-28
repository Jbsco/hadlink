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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( RawURL(..)
  , ValidURL(..)
  , ShortCode(..)
  , ClientIP(..)
  , Difficulty(..)
  , APIKey(..)
  , Nonce(..)
  , ValidationError(..)
  , APIError(..)
  , Config(..)
  ) where

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
  { cfgSecret              :: ByteString  -- HMAC secret for short codes
  , cfgPowDifficulty       :: Difficulty  -- PoW difficulty for anonymous requests (0 = disabled)
  , cfgPowDifficultyAuth   :: Difficulty  -- PoW difficulty for authenticated requests (0 = bypass)
  , cfgRateLimitPerIP      :: Int         -- Max requests per IP per window
  , cfgRateLimitWindow     :: Int         -- Rate limit window in seconds
  , cfgStoragePath         :: FilePath    -- Path to storage backend
  , cfgAPIKeys             :: [APIKey]    -- Authorized API keys
  , cfgTrustProxy          :: Bool        -- Trust X-Forwarded-For header (only enable behind trusted proxy)
  } deriving (Show, Eq, Generic)
