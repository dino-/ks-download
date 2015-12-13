-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Mongo
   ( tests )
   where

import Data.Bson
import Data.Text hiding ( map )
import Test.HUnit

import KS.Database.Mongo.Util


tests :: Test
tests = TestList $ map testLastError testData


testLastError :: (String, Document, Either String String) -> Test
testLastError (label', doc, expected) = TestCase $ do
   let actual = parseLastError doc
   assertEqual label' expected actual


testData :: [(String, Document, Either String String)]
testData =
   [ ( "getLastError ok, db operation ok"
     , [ "connectionId" =: (652 :: Int)
       , "n" =: (0 :: Int)
       , "syncMillis" =: (0 :: Int)
       , "writtenTo" =: (Nothing :: Maybe Text)
       , "err" =: (Nothing :: Maybe Text)
       , "ok" =: (1.0 :: Double)
       ]
     , Right "insertion successful"
     )
   , ( "getLastError ok, db operation NOT ok"
     , [ "connectionId" =: (652 :: Int)
       , "n" =: (0 :: Int)
       , "syncMillis" =: (0 :: Int)
       , "writtenTo" =: (Nothing :: Maybe Text)
       , "err" =: ((Just "Some error occurred with insertion!") :: Maybe Text)
       , "ok" =: (1.0 :: Double)
       ]
     , Left "insertion FAILED: Some error occurred with insertion!"
     )
   , ( "getLastError NOT ok"
     , [ "connectionId" =: (652 :: Int)
       , "n" =: (0 :: Int)
       , "syncMillis" =: (0 :: Int)
       , "writtenTo" =: (Nothing :: Maybe Text)
       , "err" =: ((Just "Should never see this") :: Maybe Text)
       , "ok" =: (2.0 :: Double)
       ]
     , Left "getLastError FAILED: ok: 2.0"
     )
   ]
