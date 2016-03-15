-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Util
   ( withRetry
   , setDate
   , isDST
   )
   where

import Control.Concurrent ( threadDelay )
import Control.Monad.Catch ( catchAll )
import Data.Time ( Day, TimeZone,
         addDays, getCurrentTime, localDay, utcToLocalTime )
import Data.Time.Calendar ( toGregorian )
import Data.Time.Calendar.WeekDate ( toWeekDate )


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


setDate :: TimeZone -> Maybe Day -> IO (Maybe Day)
setDate tz md = maybe (Just <$> twoDaysAgo tz) (return . Just) md


twoDaysAgo :: TimeZone -> IO Day
twoDaysAgo tz =
   (addDays (-2) .               -- ..two days prior to that.
      localDay .                 -- ..extract the Day..
      utcToLocalTime tz) <$>     -- ..local time for the zone the source is in..
      getCurrentTime             -- This is UTC time..


{- This computation is relative to the currently in-effect time zone
   of the system. To make sure you have it right, it can be helpful
   to set the environment variable yourself. Like this:

   $ export TZ="America/New_York"
-}
isDST :: Day -> Bool
isDST day =
   -- January, February and December are out
   if      (month < 3 || month > 11) then False

   -- April to October are in
   else if (month > 3 && month < 11) then True

   -- In March, we are DST if our previous Sunday was on or after the 8th
   else if (month == 3)              then previousSunday >= 8

   -- In November we must be before the first Sunday to be DST
   -- That means the previous Sunday must be before the 1st
   else                                   previousSunday <= 0

   where
      (_, month, monthDay) = toGregorian day

      -- Weekday in Haskell is Mo - Su, 1 - 7
      -- but we need Su - Sa, 0 - 6
      weekDay d = case toWeekDate d of
         (_, _, 7 ) -> 0
         (_, _, wd) -> wd

      previousSunday = monthDay - (weekDay day)
