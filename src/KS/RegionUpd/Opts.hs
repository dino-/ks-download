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
import System.Environment ( getEnv )
import System.FilePath ( (</>) )

import KS.Log


data Options = Options
   { optConfDir :: FilePath
   , optHelp :: Bool
   , optLogPriority :: Priority
   }

defaultOptions :: IO Options
defaultOptions = do
   homeDir <- getEnv "HOME"
   return $ Options
      { optConfDir = homeDir </> ".config" </> "kitchensnitch"
      , optHelp = False
      , optLogPriority = NOTICE
      }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['c'] ["conf-dir"]
      (ReqArg (\s opts -> opts { optConfDir = s } ) "DIR")
      "Directory containing the mongodb.conf file. Defaults to $HOME/.config/kitchensnitch"
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['p'] ["log-priority"]
      (ReqArg (\s opts -> opts { optLogPriority = read s } ) "PRIORITY")
      "Log message priority. Defaults to NOTICE"
   ]


{- Perform the actual parse of a [String]
-}
parseOpts :: [String] -> IO (Options, [String])
parseOpts args = do
   defOpts <- defaultOptions
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defOpts o, n)
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
         , "Log priorities:"
         , "  DEBUG     debugging info"
         , "  INFO      more detailed runtime info"
         , "  NOTICE    normal runtime info, a reasonable default"
         , "  EMERGENCY fatal runtime errors"
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
