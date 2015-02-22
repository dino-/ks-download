-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ks.DlInsp.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Control.Exception
import Data.List ( intercalate )
import qualified Data.Map as M
import Data.Time ( Day (..), addDays, fromGregorian, getCurrentTime,
   localDay )
import Data.Time.Zones ( loadLocalTZ, utcToLocalTimeTZ )
import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt
import Text.Regex

import Ks.DlInsp.Source.Downloaders
import Ks.DlInsp.Types


defaultOptions :: IO Options
defaultOptions = do
   tz <- loadLocalTZ     -- We expect the TZ env to be set in prod
   ut <- getCurrentTime  -- This is UTC time
   let twoDaysAgo =
         addDays (-2) .                -- ..two days ago
         localDay $                    -- ..extract the Day
         utcToLocalTimeTZ tz ut        -- The local zoned time

   return $ Options
      { optSource = ""
      , optDestDir = ""
      , optStartDate = twoDaysAgo
      , optEndDate = twoDaysAgo
      , optPageLimit = Nothing
      , optHelp = False
      }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['i'] ["insp-source"]
      (ReqArg (\s opts -> opts { optSource = s } ) "SOURCE")
      "Inspection source. Required. See SOURCE below."
   , Option ['d'] ["dest-dir"]
      (ReqArg (\s opts -> opts { optDestDir = s } ) "DESTDIR")
      "Directory for downloaded inspection JSON files. Required"
   , Option ['s']    ["start-date"]
      (ReqArg (\s opts -> opts { optStartDate = parseInputDate s } )
         "YYYYMMDD")
      "Starting date for inspection searches. Default: two days ago"
   , Option ['e']    ["end-date"]
      (ReqArg (\s opts -> opts { optEndDate = parseInputDate s } )
         "YYYYMMDD")
      "Ending date for inspection searches. Default: two days ago"
   , Option ['l'] ["page-limit"]
      (ReqArg (\l opts -> opts { optPageLimit = Just $ read l } ) "PAGES")
      "Number of pages to download (applies only to nc_wake?) Default: all of them"
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
parseOpts args = do
   handle ioError $ do
      defOpts <- defaultOptions
      case getOpt Permute options args of
         (o,n,[]  ) -> return (foldl (flip id) defOpts o, n)
         (_,_,errs) -> throwIO $ userError $ concat errs ++ usageText


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: ks-dlinsp OPTIONS"
         , "Acquire inspection data from a source"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Note: If run with no dates, you will get all of the inspections from two days ago. The idea is to give the inspection workers time to get their data into the system and is a good default for daily runs."
         , "Logging is written to stdout."
         , ""
         , "SOURCE is one of: " ++ (intercalate ", " $ M.keys downloaders)
         , ""
         , "For computing values for 'two days ago', this software will fish out the time zone for your system using the TZ environment variable if possible and /etc/localtime if necessary. On a UTC system (like production on AWS), you need to specify a TZ value from /usr/share/zoneinfo/ like this:"
         , ""
         , "   export TZ=\"America/New_York\""
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
