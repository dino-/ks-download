-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Database.Mongo.Util
   ( coll_inspections_all, coll_inspections_recent
   , coll_stats_recent
   )
   where

import Database.MongoDB ( Collection )


coll_inspections_all, coll_inspections_recent, coll_stats_recent :: Collection
coll_inspections_all    = "inspections_all"
coll_inspections_recent = "inspections_recent"
coll_stats_recent       = "stats_recent"
