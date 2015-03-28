-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module InspectionUUID
   ( tests )
   where

import Test.HUnit
import Text.Printf ( printf )

import KS.Inspection


tests :: Test
tests = TestList $ map testInspUUID testData


testInspUUID :: IdInspection -> Test
testInspUUID expected = TestCase $ do
   let label = printf "construct UUID for: %s" (show expected)
   let actual = setId . inspection $ expected
   assertEqual label (_id expected) (_id actual)


testData :: [IdInspection]
testData =
   [ IdInspection "e33231ae-b075-520e-9a29-44d3fe90a37a" $ Inspection
      { inspection_source = "nc_wake"
      , name = "Panda King"
      , addr = "3626 Rogers RD WAKE FOREST, NC 27587"
      , date = [2014,12,10]
      , score = 92.0
      , violations = 10
      , crit_violations = 3
      , reinspection = False
      , detail = ""
      }
   ]
