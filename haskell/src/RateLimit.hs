module RateLimit
  ( RateLimiter
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
