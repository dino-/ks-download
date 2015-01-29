-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ks.DlInsp.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Data.List ( intercalate )
import qualified Data.Map as M
import Data.Version ( showVersion )
import Paths_kitchensnitch_dl ( version )
import System.Console.GetOpt

import Ks.DlInsp.Source.Downloaders


data Options = Options
   { optSource :: String
   , optDestDir :: FilePath
   , optEndDate :: String
   , optDays :: Int
   , optPageLimit :: Maybe Int
   , optHelp :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optSource = ""
   , optDestDir = ""
   , optEndDate = ""
   , optDays = 30
   , optPageLimit = Nothing
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['s'] ["source"]
      (ReqArg (\s opts -> opts { optSource = s } ) "SOURCE")
      "Inspection source. Required. See SOURCE below."
   , Option ['d'] ["dest-dir"]
      (ReqArg (\s opts -> opts { optDestDir = s } ) "DESTDIR")
      "Directory for downloaded inspection JSON files. Required"
   , Option ['e']    ["end-date"]
      (ReqArg (\s opts -> opts { optEndDate = s } ) "YYYYMMDD")
      "Ending date for inspection searches. Default: today"
   , Option ['n'] ["num-days"]
      (ReqArg (\n opts -> opts { optDays = read n } ) "DAYS")
      "Number of days backwards from the ending date to acquire. Default: 30"
   , Option ['l'] ["page-limit"]
      (ReqArg (\l opts -> opts { optPageLimit = Just $ read l } ) "PAGES")
      "Number of pages to download (applies only to nc_wake?) Default: all of them"
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   ]


-- Perform the args parsing
parseOpts :: [String] -> IO (Options, [String])
parseOpts args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


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
         [ "Logging is written to stdout."
         , ""
         , "SOURCE is one of: " ++ (intercalate ", " $ M.keys downloaders)
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
