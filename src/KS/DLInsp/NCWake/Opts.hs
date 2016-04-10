-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.NCWake.Opts
   ( Options (..)
   , parseOpts, usageText
   , setDates
   )
   where

import Control.Exception
import Data.Time ( Day, fromGregorian, getCurrentTimeZone )
import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt
import Text.Regex

import KS.DLInsp.NCWake.Types ( Options (..) )
import KS.Util ( setDate )


defaultOptions :: Options
defaultOptions = Options
   { optStartDate = Nothing
   , optEndDate = Nothing
   , optPageLimit = Nothing
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['s']    ["start-date"]
      (ReqArg (\s opts -> opts { optStartDate = Just $ parseInputDate s } )
         "YYYYMMDD")
      "Starting date for inspection searches. Default: two days ago"
   , Option ['e']    ["end-date"]
      (ReqArg (\s opts -> opts { optEndDate = Just $ parseInputDate s } )
         "YYYYMMDD")
      "Ending date for inspection searches. Default: two days ago"
   , Option ['l'] ["page-limit"]
      (ReqArg (\l opts -> opts { optPageLimit = Just $ read l } ) "PAGES")
      "Number of pages to download. Default: all of them"
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   ]


parseInputDate :: String -> Day
parseInputDate str =
   case (matchRegex (mkRegex "([0-9]{4})([0-9]{2})([0-9]{2})") str) of
      Just [ys, ms, ds] -> fromGregorian (read ys) (read ms) (read ds)
      _                 -> throw $ userError $
         "Bad date format: " ++ str ++ "\n" ++ usageText


setDates :: Options -> IO Options
setDates opts = do
   tz <- getCurrentTimeZone
   newStartDate <- setDate tz $ optStartDate opts
   newEndDate <- setDate tz $ optEndDate opts
   return $ opts
      { optStartDate = newStartDate
      , optEndDate = newEndDate
      }


-- Perform the args parsing
parseOpts :: [String] -> IO (Options, [String])
parseOpts args = handle ioError $
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> throwIO $ userError $ concat errs ++ usageText


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: ks-dlinsp-dhd [OPTIONS] CONFDIR SOURCE DESTDIR"
         , "Acquire North Carolina inspection data from DHD websites"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Note: If run with no dates, you will get all of the inspections from two days ago. The idea is to give the inspection workers time to get their data into the system and is a good default for daily runs."
         , "Expects to find ks-download-SOURCE.conf in the CONFDIR specified."
         , ""
         , "SOURCE is one of: nc_wake"
         , ""
         , "DESTDIR is the directory for downloaded inspection JSON files."
         , ""
         , "For computing values for 'two days ago', this software will use the time zone set in the ks-download-SOURCE.conf file."
         , ""
         , "Logging is written to stdout."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
