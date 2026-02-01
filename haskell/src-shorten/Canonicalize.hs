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

module Canonicalize
  ( canonicalize
  , isPrivateIP
  , hasCredentials
  ) where

import Types
import SparkFFI (sparkCanonicalize)
import qualified Data.Text as T
import Network.URI
import Data.List (isPrefixOf)

-- | Canonicalize and validate a raw URL
-- Phase 2.5: Uses SPARK core via FFI for validation
canonicalize :: RawURL -> IO (Either ValidationError ValidURL)
canonicalize (RawURL rawUrl) = do
  result <- sparkCanonicalize rawUrl
  return $ case result of
    Left err -> Left err
    Right canonicalUrl -> Right (ValidURL canonicalUrl)

-- Helper functions kept for property tests

-- | Check if string represents a private IP address
isPrivateIPString :: String -> Bool
isPrivateIPString host =
  -- IPv4 private ranges
  any (`isPrefixOf` host)
    [ "10."
    , "192.168."
    , "172.16.", "172.17.", "172.18.", "172.19."
    , "172.20.", "172.21.", "172.22.", "172.23."
    , "172.24.", "172.25.", "172.26.", "172.27."
    , "172.28.", "172.29.", "172.30.", "172.31."
    , "169.254."  -- Link-local
    ] ||
  -- IPv6 private/local
  any (`isPrefixOf` host)
    [ "fc00:", "fd00:"  -- Unique local
    , "fe80:"           -- Link-local
    , "::1"             -- Loopback
    ]

-- | Check if URI authority contains credentials
hasCredentialsInAuth :: URIAuth -> Bool
hasCredentialsInAuth auth = 
  not (null (uriUserInfo auth)) && uriUserInfo auth /= ""

-- | Check if an IP address is private (for property tests)
isPrivateIP :: T.Text -> Bool
isPrivateIP = isPrivateIPString . T.unpack

-- | Check if URL has credentials (for property tests)
hasCredentials :: ValidURL -> Bool
hasCredentials (ValidURL url) =
  case parseURI (T.unpack url) of
    Just uri -> maybe False hasCredentialsInAuth (uriAuthority uri)
    Nothing -> False
