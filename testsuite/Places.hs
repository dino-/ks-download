-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Places
   ( test_computeDistance )
   where

import Test.Hspec

import KS.Data.Place ( GeoPoint (..) )
import KS.Locate.Places.Geocoding ( GeoLatLng (..) )
import KS.Locate.Places.Places ( Distance (..), computeDistance )


test_computeDistance :: SpecWith ()
test_computeDistance = describe "computeDistance" $ do
   it "identical locations are 0.0 distance apart, not NaN" $
      computeDistance inspCoords placeCoords
      `shouldBe` Distance 0.0


inspCoords :: GeoLatLng
inspCoords = GeoLatLng 35.9096536 (-78.9851382)


placeCoords :: GeoPoint
placeCoords = GeoPoint { lat = 35.9096536 , lng = (-78.9851382) }
