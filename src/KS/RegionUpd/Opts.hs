-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.RegionUpd.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt

import KS.Log


data Options = Options
   { optHelp :: Bool
   , optLogPriority :: Priority
   }

defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   , optLogPriority = NOTICE
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['p'] ["log-priority"]
      (ReqArg (\s opts -> opts { optLogPriority = read s } ) "PRIORITY")
      "Log message priority. Defaults to NOTICE"
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
         [ "Usage: ks-regionupd [OPTIONS] CONFDIR"
         , "Update the region_data and region_data_history collections with the latest statistics"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Expects to find a mongodb.conf file at the CONFDIR specified."
         , "Logging is written to stdout."
         , ""
         , "Log priorities:"
         , "  DEBUG     debugging info"
         , "  INFO      more detailed runtime info"
         , "  NOTICE    normal runtime info, a reasonable default"
         , "  EMERGENCY fatal runtime errors"
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
