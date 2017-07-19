-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Clean.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt


data Options = Options
   { optBeforeDate :: Maybe Int
   , optDelete :: Bool
   , optHelp :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optBeforeDate = Nothing
   , optDelete = False
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['b'] ["before-date"]
      (ReqArg (\s opts -> opts { optBeforeDate = Just . read $ s } ) "YYYYMMDD")
      "Check on places that were inspection on or before this data. Default: 9 months ago"
   , Option []    ["delete"]
      (NoArg (\opts -> opts { optDelete = True } ))
      "Perfom the deletions/move from the inspections_* collections. Default: false, just show what would be done."
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   ]


{- Perform the actual parse of a [String]
-}
parseOpts :: [String] -> IO (Options, [String])
parseOpts args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: ks-clean [OPTIONS] CONFDIR"
         , "Report on and possibly delete closed establishments"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Looks up places in inspections_recent that are older the date above. Reports on whether or not Google is reporting that the place is permanently closed."
         , ""
         , "The optional --delete switch above will move records for this establishment from inspections_all to inspections_archived and delete the record from inspections_recent"
         , ""
         , "Expects to find a ks-download.conf file at the CONFDIR specified."
         , "Logging is written to stdout."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
