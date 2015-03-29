-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.Config
   ( Config (..)
   , keyString
   , loadConfig )
   where

import qualified Data.Map as Map
import qualified Data.Text as T
import System.Directory ( doesFileExist )
import System.FilePath
import System.Log
import TCE.Data.ReadConf ( readConfig )

import KS.Locate.Opts


newtype GoogleKey = GoogleKey String
   deriving (Read, Show)


data Config = Config
   { logPriority :: Priority
   , googleApiKey :: GoogleKey
   , geocodingApiDelay :: Int
   , namewordsStopwords :: [T.Text]
   , namewordsSpecialCases :: Map.Map T.Text [T.Text]
   , placesTypes :: [String]
   }
   deriving (Read, Show)


loadConfig :: Options -> IO Config
loadConfig options = do
   let confPath = (optConfDir options) </> "ksdl.conf"
   conf <- (either error id . readConfig) `fmap` readFile confPath

   -- A Google API key in a file by itself will supercede the one
   -- in the conf file
   maybe (return conf) (\k -> return $ conf { googleApiKey = k })
      =<< loadGoogleKey options


-- Google Places API key
loadGoogleKey :: Options -> IO (Maybe GoogleKey)
loadGoogleKey options = do
   let keyPath = (optConfDir options) </> "GoogleAPIKey"
   exists <- doesFileExist keyPath
   if exists
      then do
         k <- (GoogleKey .  -- ..and construct the proper type
            unwords . words) `fmap`  -- ..strip any trailing whitespace
            (readFile keyPath)
         return $ Just k
      else return Nothing


keyString :: GoogleKey -> String
keyString (GoogleKey ks) = ks
