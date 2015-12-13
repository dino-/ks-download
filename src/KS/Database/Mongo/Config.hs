-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Database.Mongo.Config
   ( Config (..)
   , loadConfig )
   where

import qualified Data.Text as T
import System.FilePath
import TCE.Data.ReadConf ( readConfig )


data Config = Config
   { mongoServerIP :: String
   , mongoUsername :: T.Text
   , mongoPassword :: T.Text
   , mongoDatabase :: T.Text
   }
   deriving (Read, Show)


loadConfig :: FilePath -> IO Config
loadConfig confDir = do
   let confPath = confDir </> "mongodb.conf"
   (either error id . readConfig) `fmap` readFile confPath
