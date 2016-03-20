-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Database.Mongo.Util
   ( coll_inspections_all, coll_inspections_recent
   , coll_stats_recent
   , parseLastError
   )
   where

import Data.Bson
import qualified Data.Text as T
import Database.MongoDB ( Collection )
import Text.Printf ( printf )


coll_inspections_all, coll_inspections_recent, coll_stats_recent :: Collection
coll_inspections_all    = "inspections_all"
coll_inspections_recent = "inspections_recent"
coll_stats_recent       = "stats_recent"


parseLastError :: Document -> Either String String
parseLastError errdoc = lastErrStatus >> atE "err"
   where
      lastErrStatus :: Either String String
      lastErrStatus = case at "ok" errdoc of
         1.0   -> Right "getLastError successful"
         c     -> Left $ printf "getLastError FAILED: ok: %f" (c :: Double)

      atE :: T.Text -> Either String String
      atE key = case at key errdoc of
         Just msg -> Left $ printf "insertion FAILED: %s" (T.unpack msg)
         Nothing  -> Right "insertion successful"
