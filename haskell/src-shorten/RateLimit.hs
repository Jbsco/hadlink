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
  , TokenBucket
  , newRateLimiter
  , checkLimit
  , cleanupOldBuckets
  ) where

import Types
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Data.Time.Clock

-- | Token bucket for a single client
data TokenBucket = TokenBucket
  { tbTokens     :: !Double   -- Current token count (fractional for smooth refill)
  , tbLastAccess :: !UTCTime  -- Last access time (for refill calculation and cleanup)
  } deriving (Show, Eq)

-- | Token bucket rate limiter with per-IP tracking
data RateLimiter = RateLimiter
  { rlBuckets   :: TVar (Map.Map ClientIP TokenBucket)
  , rlMaxTokens :: !Double    -- Maximum tokens (bucket capacity)
  , rlRefillRate :: !Double   -- Tokens added per second
  , rlWindowSecs :: !Int      -- Window for cleanup (buckets older than this are removed)
  }

-- | Create a new rate limiter from config
-- maxRequests = bucket capacity, windowSeconds = time to fully refill
newRateLimiter :: Config -> IO RateLimiter
newRateLimiter config = do
  buckets <- newTVarIO Map.empty
  let maxTokens = fromIntegral (cfgRateLimitPerIP config)
      windowSecs = cfgRateLimitWindow config
      -- Refill rate: fill bucket completely over the window period
      refillRate = maxTokens / fromIntegral windowSecs
  return $ RateLimiter buckets maxTokens refillRate windowSecs

-- | Check if a request from the given IP is allowed
-- Returns True if allowed (and consumes a token), False if rate limited
checkLimit :: RateLimiter -> ClientIP -> IO Bool
checkLimit limiter clientIP = do
  now <- getCurrentTime
  atomically $ do
    buckets <- readTVar (rlBuckets limiter)
    let bucket = Map.lookup clientIP buckets
        (allowed, newBucket) = processRequest limiter now bucket
        newBuckets = Map.insert clientIP newBucket buckets
    writeTVar (rlBuckets limiter) newBuckets
    return allowed

-- | Process a request: refill tokens, check availability, consume if allowed
processRequest :: RateLimiter -> UTCTime -> Maybe TokenBucket -> (Bool, TokenBucket)
processRequest limiter now maybeBucket =
  case maybeBucket of
    Nothing ->
      -- New client: start with full bucket minus one token
      let initialTokens = rlMaxTokens limiter - 1.0
      in (True, TokenBucket initialTokens now)

    Just bucket ->
      let -- Calculate time since last access
          elapsed = realToFrac (diffUTCTime now (tbLastAccess bucket)) :: Double
          -- Add tokens based on elapsed time (capped at max)
          refilled = min (rlMaxTokens limiter) (tbTokens bucket + elapsed * rlRefillRate limiter)
      in if refilled >= 1.0
         then -- Allow request, consume one token
              (True, TokenBucket (refilled - 1.0) now)
         else -- Deny request, keep current token count but update access time
              (False, TokenBucket refilled now)

-- | Remove buckets that haven't been accessed recently
-- Call this periodically to prevent unbounded memory growth
cleanupOldBuckets :: RateLimiter -> IO Int
cleanupOldBuckets limiter = do
  now <- getCurrentTime
  let maxAge = fromIntegral (rlWindowSecs limiter * 2) :: NominalDiffTime
      cutoff = addUTCTime (negate maxAge) now
  atomically $ do
    buckets <- readTVar (rlBuckets limiter)
    let (old, current) = Map.partition (\b -> tbLastAccess b < cutoff) buckets
        removedCount = Map.size old
    writeTVar (rlBuckets limiter) current
    return removedCount
