-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.RegionUpd.Opts
   ( Options (..)
   , defaultOptions
   , parseOpts, usageText
   )
   where

import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt


data Options = Options
   { optConfDir :: FilePath
   , optHelp :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optConfDir = "."
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['c'] ["conf-dir"]
      (ReqArg (\s opts -> opts { optConfDir = s } ) "DIR")
      "Directory containing the mongodb.conf file. Defaults to ."
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
         [ "Usage: ks-regionupd [OPTIONS]"
         , "Update the region_data and region_data_history collections with the latest statistics"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Logging is written to stdout."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
