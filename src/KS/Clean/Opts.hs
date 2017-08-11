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
   , optArchive :: Bool
   , optHelp :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optBeforeDate = Nothing
   , optArchive = False
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['b'] ["before-date"]
      (ReqArg (\s opts -> opts { optBeforeDate = Just . read $ s } ) "YYYYMMDD")
      "Check on places that were inspection on or before this data. Default: 9 months ago"
   , Option []    ["archive"]
      (NoArg (\opts -> opts { optArchive = True } ))
      "Archive closed places. See ARCHIVING below. Default: false, just show what would be done."
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
         , "Expects to find a ks-download.conf file at the CONFDIR specified."
         , "Logging is written to stdout."
         , ""
         , "ARCHIVING"
         , ""
         , "Archiving (--archive switch) means, for each Places ID of a closed establishment, this will be done:"
         , ""
         , "  - All inspections for that Places ID will be inserted into the inspections_archived collection"
         , "  - All inspections for that Places ID will be deleted from the inspections_all collection"
         , "  - The one inspection for that Places ID will be deleted from the inspections_recent collection"
         , ""
         , "A place is determined to be closed by looking it up via the Google Places API."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
