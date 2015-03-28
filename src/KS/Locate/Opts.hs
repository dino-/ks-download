-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.Opts
   ( Options (..)
   , defaultOptions
   , parseOpts, usageText
   )
   where

import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt


data Options = Options
   { optSuccessDir :: Maybe FilePath
   , optFailDir :: Maybe FilePath
   , optDelete :: Bool
   , optConfDir :: FilePath
   , optHelp :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optSuccessDir = Nothing
   , optFailDir = Nothing
   , optDelete = False
   , optConfDir = "."
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['s'] ["success-dir"]
      (ReqArg (\s opts -> opts { optSuccessDir = Just s } ) "SUCCDIR")
      "Directory for successful lookups. Optional. If missing, JSON is sent to stdout."
   , Option ['f'] ["fail-dir"]
      (ReqArg (\s opts -> opts { optFailDir = Just s } ) "FAILDIR")
      "Directory for failures. Optional. If missing, failure JSON is not copied anywhere."
   , Option []    ["delete"]
      (NoArg (\opts -> opts { optDelete = True } ))
      "Delete source files as they're processed. BE CAREFUL, this will delete even if no above dest dirs are supplied."
   , Option ['c'] ["conf-dir"]
      (ReqArg (\s opts -> opts { optConfDir = s } ) "CONFDIR")
      "Directory to load ksdl.conf and GoogleAPIKey files from. Defaults to ."
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
         [ "Usage: ks-locate [OPTIONS] FILE|DIR"
         , "Look up inspections with Google Geocoding and Places"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Looks up the file or dir full of files specified"
         , "Writes successful lookups to SUCCDIR or stdout if omitted"
         , "Writes failed lookup input files to FAILDIR"
         , "Expects to find a ./ksdl.conf file, or at the CONFDIR specified."
         , "Logging is written to stdout."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
