-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DBInsert.Opts
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


data Options = Options
   { optConfDir :: FilePath
   , optHelp :: Bool
   }

defaultOptions :: IO Options
defaultOptions = do
   homeDir <- getEnv "HOME"
   return $ Options
      { optConfDir = homeDir </> ".config" </> "kitchensnitch"
      , optHelp = False
      }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['c'] ["conf-dir"]
      (ReqArg (\s opts -> opts { optConfDir = s } ) "DIR")
      "Directory containing the mongodb.conf file. Defaults to $HOME/.config/kitchensnitch"
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
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
         [ "Usage: ks-dbinsert [OPTIONS] FILE|DIR"
         , "Insert inspections + Places info into MongoDB"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Looks up the file or dir full of files specified"
         , "Logging is written to stdout."
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]
