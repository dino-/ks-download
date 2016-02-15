-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Util
   ( withRetry
   , setDates
   )
   where

import Control.Concurrent ( threadDelay )
import Control.Monad.Catch ( catchAll )
import Data.Time ( Day, TimeZone,
         addDays, getCurrentTime, localDay, utcToLocalTime )

import KS.DLInsp.Types ( Options (optStartDate, optEndDate) )


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


setDates :: TimeZone -> Options -> IO Options
setDates tz opts = do
   newStartDate <- setDate tz $ optStartDate opts
   newEndDate <- setDate tz $ optEndDate opts
   return $ opts
      { optStartDate = newStartDate
      , optEndDate = newEndDate
      }


setDate :: TimeZone -> Maybe Day -> IO (Maybe Day)
setDate tz md = maybe (Just <$> twoDaysAgo tz) (return . Just) md


twoDaysAgo :: TimeZone -> IO Day
twoDaysAgo tz =
   (addDays (-2) .               -- ..two days prior to that.
      localDay .                 -- ..extract the Day..
      utcToLocalTime tz) <$>     -- ..local time for the zone the source is in..
      getCurrentTime             -- This is UTC time..
