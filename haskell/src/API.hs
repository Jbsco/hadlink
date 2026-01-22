{-# LANGUAGE OverloadedStrings #-}

module API
  ( app
  , createHandler
  , resolveHandler
  ) where

import Types
import Canonicalize (canonicalize)
import ShortCode (generateShortCode)
import Store
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Network.Wai
import Network.HTTP.Types
import Data.Aeson (object, (.=), encode)

-- | WAI Application
-- TODO: Implement full routing, middleware, error handling
app :: Config -> SQLiteStore -> Application
app config store req respond = do
  case (requestMethod req, pathInfo req) of
    ("POST", ["api", "create"]) -> createHandler config store req respond
    ("GET", [code]) -> resolveHandler store code req respond
    _ -> respond $ responseLBS status404 [] "Not found"

-- | Handle short link creation
createHandler :: Config -> SQLiteStore -> Application
createHandler config store req respond = do
  -- TODO: 
  -- 1. Parse request body
  -- 2. Extract API key from headers
  -- 3. Check rate limit
  -- 4. Verify PoW if required
  -- 5. Canonicalize URL
  -- 6. Generate short code
  -- 7. Store mapping
  -- 8. Return short link
  respond $ responseLBS status501 [] "Not implemented yet"

-- | Handle redirect resolution
resolveHandler :: SQLiteStore -> T.Text -> Application
resolveHandler store code req respond = do
  -- TODO:
  -- 1. Validate code format
  -- 2. Lookup in store
  -- 3. Return 302 redirect or 404
  respond $ responseLBS status501 [] "Not implemented yet"
