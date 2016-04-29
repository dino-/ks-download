-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Util
   ( withRetry
   , today
   , twoDaysAgo
   , dateIntToDay
   )
   where

import Control.Concurrent ( threadDelay )
import Control.Monad.Catch ( catchAll )
import Data.Time ( Day, TimeZone,
         addDays, getCurrentTime, localDay, utcToLocalTime )
import Data.Time.Calendar ( fromGregorian )


withRetry :: Int -> Int -> IO a -> (String -> IO ()) -> IO (Either String a)
withRetry tryNumber delay action logF = withRetry' tryNumber where
   withRetry' 0          = return . Left $ "Failed because retries are exhausted"
   withRetry' tryNumber' = do
      response <- catchAll (Right <$> action) (return . Left . show)
      case response of
         r@(Right _) -> return r
         Left e      -> do
            logF $ "Failed, retrying. Error: " ++ e
            threadDelay $ delay * 1000 * 1000
            withRetry' $ tryNumber' - 1


today :: TimeZone -> IO Day
today = nDaysAgo 0


twoDaysAgo :: TimeZone -> IO Day
twoDaysAgo = nDaysAgo 2


nDaysAgo :: Integer -> TimeZone -> IO Day
nDaysAgo daysAgo tz =
   (addDays (negate daysAgo) .   -- ..daysAgo days prior to that.
      localDay .                 -- ..extract the Day..
      utcToLocalTime tz) <$>     -- ..local time for the zone the source is in..
      getCurrentTime             -- This is UTC time..


dateIntToDay :: Int -> Day
dateIntToDay dateInt = fromGregorian year month day
   where
      dateStr = show dateInt
      year = read . take 4 $ dateStr
      month = read . take 2 . drop 4 $ dateStr
      day = read . drop 6 $ dateStr
