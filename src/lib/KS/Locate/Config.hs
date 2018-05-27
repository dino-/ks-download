-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.Config
   ( Config (..)
   , loadConfig
   , keyString
   )
   where

import System.FilePath
import System.Log
import TCE.Data.ReadConf ( readConfig )


newtype GoogleKey = GoogleKey String
   deriving (Read, Show)


data Config = Config
   { logPriority :: Priority
   , googleApiKey :: GoogleKey
   , geocodingApiDelay :: Int
   }
   deriving (Read, Show)


loadConfig :: FilePath -> IO Config
loadConfig confDir = do
   let confPath = confDir </> "ks-download.conf"
   (either error id . readConfig) <$> readFile confPath


keyString :: GoogleKey -> String
keyString (GoogleKey ks) = ks
