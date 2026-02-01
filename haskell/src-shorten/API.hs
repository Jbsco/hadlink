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

module API
  ( app
  , selectDifficulty
  , resolveHandler
  ) where

import API.Shorten (shortenApp, selectDifficulty)
import API.Resolve (resolveHandler)
import Types
import Store
import RateLimit (RateLimiter)
import Logging (Logger)
import Network.Wai (Application)

-- | WAI Application (delegates to shortenApp)
app :: Config -> SQLiteStore -> RateLimiter -> Logger -> Application
app = shortenApp
