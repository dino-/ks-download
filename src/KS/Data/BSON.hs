-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for inserting inspections into MongoDB
-}

module KS.Data.BSON
   ( docToBSON )
   where

import Data.Bson
import Data.Geospatial ( GeoPoint (..) )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import qualified Data.Text as T

import qualified KS.Data.Document as D
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P


-- This is our KS.Data.Document -> Data.BSON.Document
docToBSON :: D.Document -> Document
docToBSON doc = bdoc
   where
      insp = D.inspection doc
      pl = D.place doc
      (GeoPoint coords) = P.location pl
      bdoc =
         [ "doctype" =: D.doctype doc
         , "inspection" =:
            [ "inspection_source" =: I.inspection_source insp
            , "name" =: I.name insp
            , "addr" =: I.addr insp
            , "date" =: ((round . utcTimeToPOSIXSeconds . I.date
               $ insp) :: Integer)
            , "score" =: I.score insp
            , "violations" =: I.violations insp
            , "crit_violations" =: I.crit_violations insp
            , "reinspection" =: I.reinspection insp
            , "detail" =: I.detail insp
            ]
         , "place" =:
            [ "name" =: P.name pl
            , "vicinity" =: P.vicinity pl
            , "location" =:
               [ "type" =: ("Point" :: T.Text)
               , "coordinates" =: coords
               ]
            , "types" =: P.types pl
            , "place_id" =: P.place_id pl
            ]
         ]
