-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Database.Mongo.Config
   ( MongoConfig (..)
   , loadMongoConfig )
   where

import qualified Data.Text as T
import System.FilePath
import TCE.Data.ReadConf ( readConfig )


data MongoConfig = MongoConfig
   { ip       :: String
   , username :: T.Text
   , password :: T.Text
   , database :: T.Text
   }
   deriving (Read, Show)


loadMongoConfig :: FilePath -> IO MongoConfig
loadMongoConfig confDir = do
   let confPath = confDir </> "mongodb.conf"
   (either error id . readConfig) `fmap` readFile confPath
