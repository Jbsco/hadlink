{-# LANGUAGE OverloadedStrings #-}

module Canonicalize
  ( canonicalize
  , isPrivateIP
  , hasCredentials
  ) where

import Types
import qualified Data.Text as T
import Network.URI
import Data.Char (toLower)
import Data.List (isPrefixOf)

-- | Canonicalize and validate a raw URL
-- Phase 1: Pure Haskell implementation
-- Phase 2: This will be replaced with FFI call to SPARK
canonicalize :: RawURL -> Either ValidationError ValidURL
canonicalize (RawURL rawUrl) = do
  -- Check length
  when (T.length rawUrl > 2048) $
    Left InvalidLength
  
  when (T.null rawUrl) $
    Left InvalidLength
  
  -- Parse URI
  uri <- case parseURI (T.unpack rawUrl) of
    Nothing -> Left (ParseError "Invalid URI syntax")
    Just u  -> Right u
  
  -- Validate scheme
  let scheme = map toLower (uriScheme uri)
  unless (scheme == "http:" || scheme == "https:") $
    Left InvalidScheme
  
  -- Get authority
  auth <- case uriAuthority uri of
    Nothing -> Left InvalidHost
    Just a  -> Right a
  
  -- Check for credentials
  when (hasCredentialsInAuth auth) $
    Left CredentialsPresent
  
  -- Check for private/local addresses
  let host = uriRegName auth
  when (isPrivateHost host) $
    Left PrivateAddress
  
  -- Canonicalize: lowercase scheme and host, remove default ports
  let canonicalUri = normalizeURI uri
  
  return $ ValidURL (T.pack $ uriToString id canonicalUri "")
  where
    when True action = action
    when False _ = Right ()
    unless False _ = Right ()
    unless True action = action

-- | Check if URI authority contains credentials
hasCredentialsInAuth :: URIAuth -> Bool
hasCredentialsInAuth auth = 
  not (null (uriUserInfo auth)) && uriUserInfo auth /= ""

-- | Check if host is a private or local address
isPrivateHost :: String -> Bool
isPrivateHost host =
  isLocalhost host || isPrivateIPString host

-- | Check if host is localhost
isLocalhost :: String -> Bool
isLocalhost h = 
  h == "localhost" || 
  h == "127.0.0.1" || 
  h == "::1" ||
  ".local" `isPrefixOf` reverse h

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

-- | Normalize URI (lowercase scheme/host, remove default ports)
normalizeURI :: URI -> URI
normalizeURI uri = uri
  { uriScheme = map toLower (uriScheme uri)
  , uriAuthority = fmap normalizeAuth (uriAuthority uri)
  }
  where
    normalizeAuth auth = auth
      { uriRegName = map toLower (uriRegName auth)
      , uriPort = normalizePort (uriScheme uri) (uriPort auth)
      }
    
    normalizePort "http:" ":80" = ""
    normalizePort "https:" ":443" = ""
    normalizePort _ port = port

-- | Check if an IP address is private (for property tests)
isPrivateIP :: T.Text -> Bool
isPrivateIP = isPrivateIPString . T.unpack

-- | Check if URL has credentials (for property tests)
hasCredentials :: ValidURL -> Bool
hasCredentials (ValidURL url) =
  case parseURI (T.unpack url) of
    Just uri -> case uriAuthority uri of
      Just auth -> hasCredentialsInAuth auth
      Nothing -> False
    Nothing -> False
