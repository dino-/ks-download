-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DBInsert.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Console.GetOpt


data Options = Options
   { optHelp :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['h'] ["help"]
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
         [ "Usage: ks-dbinsert [OPTIONS] CONFDIR FILE|DIR"
         , "Insert inspections + Places info into MongoDB"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Looks up the FILE or DIR full of files specified"
         , "Expects to find a mongodb.conf file at the CONFDIR specified."
         , "Logging is written to stdout."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
