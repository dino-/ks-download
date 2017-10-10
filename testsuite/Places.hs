-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Places
   ( test_computeDistance )
   where

import Test.Hspec

import KS.Data.Place ( GeoPoint (GeoPoint), lat, lng )
import KS.Locate.Places.Places ( Distance (..), computeDistance )


coords1 :: GeoPoint
coords1 = GeoPoint { lat = 35.9096536 , lng = (-78.9851382) }


test_computeDistance :: SpecWith ()
test_computeDistance = describe "computeDistance" $ do
   it "identical locations are 0.0 distance apart, not NaN" $
      computeDistance coords1 coords1 `shouldBe` Distance 0.0
