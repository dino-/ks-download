-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Database.Mongo.Util
   ( coll_feedback
   , coll_inspections_all, coll_inspections_archived
   , coll_inspections_recent, coll_stats_recent
   , mongoConnect
   )
   where

import Data.Text ( Text )
import Database.MongoDB ( Collection, Host (..), Pipe,
   PortID (PortNumber), access, auth, connect, slaveOk )

import qualified KS.Database.Mongo.Config as MC
import KS.Log ( lname, noticeM )


coll_feedback, coll_inspections_all, coll_inspections_archived,
   coll_inspections_recent, coll_stats_recent :: Collection

coll_feedback              = "feedback"
coll_inspections_all       = "inspections_all"
coll_inspections_archived  = "inspections_archived"
coll_inspections_recent    = "inspections_recent"
coll_stats_recent          = "stats_recent"


mongoConnect :: FilePath -> IO (Pipe, Text)
mongoConnect confDir = do
   mongoConf <- MC.loadMongoConfig confDir

   -- Get a connection to Mongo, they call it a 'pipe'
   pipe <- connect $ Host (MC.ip mongoConf)
      (PortNumber . fromIntegral . MC.port $ mongoConf)

   -- Authenticate with mongo, log the auth state
   (access pipe slaveOk (MC.database mongoConf)
      $ auth (MC.username mongoConf) (MC.password mongoConf)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)

   return (pipe, MC.database mongoConf)
