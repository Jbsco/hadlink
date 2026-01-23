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

module RateLimit
  ( RateLimiter
  , TokenBucket(MkTokenBucket)
  , newRateLimiter
  , checkLimit
  ) where

import Types
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Data.Time.Clock

-- | Token bucket rate limiter
-- TODO: Implement fixed-size hash table with aging
data RateLimiter = RateLimiter
  { _rlBuckets :: TVar (Map.Map ClientIP TokenBucket)
  , _rlConfig  :: Config
  }

newtype TokenBucket = MkTokenBucket UTCTime

-- | Create a new rate limiter
newRateLimiter :: Config -> IO RateLimiter
newRateLimiter config = do
  buckets <- newTVarIO Map.empty
  return $ RateLimiter buckets config

-- | Check if request is allowed under rate limit
checkLimit :: RateLimiter -> ClientIP -> IO Bool
checkLimit _limiter _ip = do
  -- TODO: Implement token bucket algorithm
  -- 1. Get or create bucket for IP
  -- 2. Refill tokens based on time elapsed
  -- 3. Check if tokens available
  -- 4. Consume token if available
  return True  -- Placeholder: allow all for now
