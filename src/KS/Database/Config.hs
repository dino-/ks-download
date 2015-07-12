-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Database.Config
   ( Config (..)
   , loadConfig )
   where

import qualified Data.Text as T
import System.FilePath
import TCE.Data.ReadConf ( readConfig )

import KS.Database.Opts


data Config = Config
   { mongoServerIP :: String
   , mongoUsername :: T.Text
   , mongoPassword :: T.Text
   , mongoDatabase :: T.Text
   , mongoCollection :: T.Text
   }
   deriving (Read, Show)


loadConfig :: Options -> IO Config
loadConfig options = do
   let confPath = (optConfDir options) </> "ks-dbinsert.conf"
   (either error id . readConfig) `fmap` readFile confPath
