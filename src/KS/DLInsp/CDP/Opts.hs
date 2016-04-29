-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.CDP.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Control.Exception
import Data.Time ( Day, fromGregorian )
import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt
import Text.Regex

import KS.DLInsp.CDP.Types ( Options (..) )


defaultOptions :: Options
defaultOptions = Options
   { optStartDate = Nothing
   , optEndDate = Nothing
   , optName = Nothing
   , optEstNames = False
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['s']    ["start-date"]
      (ReqArg (\s opts -> opts { optStartDate = Just $ parseInputDate s } )
         "YYYYMMDD")
      "Starting date for inspection searches. Default: date of last inspection in the db or two days ago if that's not available"
   , Option ['e']    ["end-date"]
      (ReqArg (\s opts -> opts { optEndDate = Just $ parseInputDate s } )
         "YYYYMMDD")
      "Ending date for inspection searches. Default: today"
   , Option ['n'] ["name"]
      (ReqArg (\s opts -> opts { optName = Just s } ) "ESTABLISHMENT_NAME")
      "Retrieve inspections for a particular named establishment"
   , Option [] ["est-names"]
      (NoArg (\opts -> opts { optEstNames = True } ))
      "Retrieve a list of establishment names that have inspections between the specified dates. Default: false"
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
         [ "Usage: ks-dlinsp-cdp [OPTIONS] CONFDIR SOURCE DESTDIR"
         , "Acquire North Carolina inspection data from CDP websites"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Note: If run with no dates, you will get all of the inspections from the past two days. The idea is to give the inspection workers time to get their data into the system and is a good default for daily runs."
         , "Expects to find ks-download-SOURCE.conf in the CONFDIR specified."
         , ""
         , "The --name and --est-names switches are intended to be used for gathering historical data as opposed to 'dailies' of the latest inspections in a date range."
         , ""
         , "SOURCE is one of: nc_chatham, nc_durham, nc_orange"
         , ""
         , "DESTDIR is the directory for downloaded inspection JSON files."
         , ""
         , "For computing values for 'today' and 'two days ago', this software will use the time zone set in the ks-download-SOURCE.conf file."
         , ""
         , "Logging is written to stdout."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
