-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Places
   ( test_computeDistance )
   where

import Test.Hspec

import KS.Data.Place ( GeoPoint (..), Place (..) )
import KS.Locate.Places.Geocoding ( GeoLatLng (..) )
import KS.Locate.Places.Places ( Distance (..), computeDistance )


test_computeDistance :: SpecWith ()
test_computeDistance = describe "computeDistance" $ do
   it "identical locations are 0.0 distance apart" $
      computeDistance inspCoords placeJerseyMikes
      `shouldBe` ((Distance 0.0), placeJerseyMikes)


inspCoords :: GeoLatLng
inspCoords = GeoLatLng 35.9096536 (-78.9851382)


placeJerseyMikes :: Place
placeJerseyMikes = Place
   { name = "Jersey Mike's Subs"
   , vicinity = "6118-C Farrington Road, Falconbridge Center, Chapel Hill"
   , location = GeoPoint
      { lat = 35.9096536
      , lng = (-78.9851382)
      }
   , types = [ "meal_takeaway", "restaurant", "food", "store",
      "point_of_interest", "establishment" ]
   , place_id = "ChIJe4Aj1GforIkR_ETQmiusfHk"
   }
