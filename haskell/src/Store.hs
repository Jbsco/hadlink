{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Store
  ( SQLiteStore
  , openStore
  , closeStore
  , initializeStore
  , put
  , get
  , exists
  , withStore
  ) where

import Types
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple (Connection)
import Control.Exception (bracket)

-- | SQLite-based storage implementation
newtype SQLiteStore = SQLiteStore Connection

-- | Open a SQLite store
openStore :: FilePath -> IO SQLiteStore
openStore path = do
  conn <- SQL.open path
  -- Enable WAL mode for better concurrency
  SQL.execute_ conn "PRAGMA journal_mode=WAL"
  -- Set synchronous to NORMAL for better performance
  SQL.execute_ conn "PRAGMA synchronous=NORMAL"
  return $ SQLiteStore conn

-- | Close the store
closeStore :: SQLiteStore -> IO ()
closeStore (SQLiteStore conn) = SQL.close conn

-- | Initialize the database schema
initializeStore :: SQLiteStore -> IO ()
initializeStore (SQLiteStore conn) = do
  -- Create table if it doesn't exist
  SQL.execute_ conn $ SQL.Query $ T.pack $
    "CREATE TABLE IF NOT EXISTS links (\n\
    \  short_code TEXT PRIMARY KEY NOT NULL,\n\
    \  canonical_url TEXT NOT NULL,\n\
    \  created_at INTEGER NOT NULL DEFAULT (strftime('%s', 'now'))\n\
    \)"

  -- Create index on created_at for potential cleanup/analytics
  SQL.execute_ conn $ SQL.Query $ T.pack $
    "CREATE INDEX IF NOT EXISTS idx_created_at ON links(created_at)"

-- | Store a short code -> URL mapping
put :: ShortCode -> ValidURL -> SQLiteStore -> IO ()
put shortCode url (SQLiteStore conn) = do
  let ShortCode code = shortCode
      ValidURL urlText = url
  -- INSERT OR IGNORE for idempotency
  SQL.execute conn
    "INSERT OR IGNORE INTO links (short_code, canonical_url) VALUES (?, ?)"
    (code, urlText)

-- | Lookup a URL by short code
get :: ShortCode -> SQLiteStore -> IO (Maybe ValidURL)
get shortCode (SQLiteStore conn) = do
  let ShortCode code = shortCode
  results <- SQL.query conn
    "SELECT canonical_url FROM links WHERE short_code = ? LIMIT 1"
    (SQL.Only code) :: IO [SQL.Only T.Text]
  case results of
    [SQL.Only url] -> return $ Just (ValidURL url)
    _ -> return Nothing

-- | Check if a short code exists
exists :: ShortCode -> SQLiteStore -> IO Bool
exists shortCode (SQLiteStore conn) = do
  let ShortCode code = shortCode
  results <- SQL.query conn
    "SELECT 1 FROM links WHERE short_code = ? LIMIT 1"
    (SQL.Only code) :: IO [SQL.Only Int]
  return $ not (null results)

-- | Helper to run store operations with automatic cleanup
withStore :: FilePath -> (SQLiteStore -> IO a) -> IO a
withStore path = bracket (openStore path) closeStore
