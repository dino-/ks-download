-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Util
   ( TryCount (Remaining)
   , Seconds (..)
   , dateIntToDay
   , dayToDateInt
   , nDaysAgo
   , today
   , twoDaysAgo
   , withRetry
   )
   where

import Control.Concurrent ( threadDelay )
import Control.Monad.Catch ( catchAll )
import Data.Time ( Day, TimeZone,
         addDays, getCurrentTime, localDay, utcToLocalTime )
import Data.Time.Calendar ( fromGregorian, toGregorian )
import Text.Printf ( printf )


data TryCount
  = NoneLeft
  | Remaining Int

decrementTry :: TryCount -> TryCount
decrementTry NoneLeft = NoneLeft
decrementTry (Remaining n)
  | n > 1 = Remaining $ n - 1
  | otherwise = NoneLeft


newtype Seconds = Seconds Int


withRetry :: TryCount -> Seconds -> IO a -> (String -> IO ()) -> IO (Either String a)
withRetry tryCount (Seconds delay) action logF = withRetry' tryCount where
   withRetry' NoneLeft = return . Left $ "Failed because retries are exhausted"
   withRetry' remaining = do
      response <- catchAll (Right <$> action) (return . Left . show)
      case response of
         r@(Right _) -> return r
         Left e      -> do
            logF $ "Failed, retrying. Error: " ++ e
            threadDelay $ delay * 1000 * 1000
            withRetry' $ decrementTry remaining


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


dayToDateInt :: Day -> Int
dayToDateInt dayS = read $ printf "%4d%02d%02d" year month day
   where (year, month, day) = toGregorian dayS
