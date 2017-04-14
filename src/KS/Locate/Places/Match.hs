-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module KS.Locate.Places.Match
   ( Match, match
   , test_cleanAddress
   )
   where

import Data.Attoparsec.Text hiding ( count, match )
import Data.Char ( isDigit )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import Prelude hiding ( takeWhile )
import Test.Hspec

import KS.Locate.Locate
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P
import KS.Log


type Match = (I.Inspection, P.Place)

type MatchInternal = (Bool, Match)


match :: [P.Place] -> KSDL Match
match ps = do
   insp <- asks getInspection
   let mis = map (combine insp) ps
   let count = (sum . map bToI $ mis) :: Int

   when (count == 0) $ do
      throwError $ ErrMsg ERROR "ERROR Match: No Places result matches"

   liftIO $ do
      noticeM lname "Matches:"
      mapM_ (noticeM lname) $ catMaybes $ map fmtMatched mis

   when (count > 1) $ liftIO $ do
      warningM lname "WARNING Match: More than one Places result matched"

   return . head . catMaybes . map positiveMatch $ mis

   where
      {- Combine the inspection, places result and a boolean
         indicating whether or not we think they refer to the
         same place.

         The cleaned-up Places address is returned back to us by
         isMatch and substituted into the Place data type here.
      -}
      combine :: I.Inspection -> P.Place -> MatchInternal
      combine insp pl = (matched, (insp, pl { P.vicinity = newPvic }))
         where
            (matched, newPvic) =
               isMatch (I.addr insp) (P.vicinity pl)

      bToI :: MatchInternal -> Int
      bToI (True,  (_, _)) = 1
      bToI (False, (_, _)) = 0

      fmtMatched :: MatchInternal -> Maybe String
      fmtMatched (True , (_, pl)) = Just . T.unpack . TL.toStrict $
         TF.format "{} | {}" ((P.name pl), (P.vicinity pl))
      fmtMatched (False, (_, _ )) = Nothing

      positiveMatch :: MatchInternal -> Maybe Match
      positiveMatch (True , m) = Just m
      positiveMatch (False, _) = Nothing


{- Determine if two addresses are a "match" based on the beginning
   digits. Given how close we get with Google Place search, this
   gets us the rest of the way to disambiguate the hits.

   In addition to a True/False match status, we return the cleaned-up
   address that was computed below with removePrefixZip. This is
   so we can show our users the true address.
-}
isMatch :: T.Text -> T.Text -> (Bool, T.Text)
isMatch iaddr pvic =
   if (not . T.null $ pIaddr) && (not . T.null $ pNewPvic)
         && (prefix iaddr == prefix newPvic)
      then (True, newPvic)
      else (False, newPvic)

   where
      pIaddr = prefix iaddr
      pNewPvic = prefix newPvic
      newPvic = cleanAddress pvic
      prefix = T.takeWhile isDigit




{- We get these ridiculous addresses from Google Places where they've
   clearly mistakenly put the zip code up front or list a building 
   first. They look like this:

      "27603, 7900 Fayetteville Road, Raleigh"
      "Wells Fargo Capital Center, 150 Fayetteville St, Raleigh"

   This group of functions returns the address string with this
   garbage removed, up to the street address number.

   What we want:

   - If the string doesn't start with a number
      - Remove everything up to and including the first occurrance of ", "
   - If the string is a number
      - removePrefixZip function (i.e. digits up to and including ", ")
-}

cleanAddress :: T.Text -> T.Text
cleanAddress pvic
   -- The address is empty! Ugh but just send it back.
   | T.null pvic = pvic

   -- Starts with a number, send to the pre-zip-code remover
   | isDigit $ T.head pvic = removePrefixZip pvic

   -- No number, send to the thing that tries to remove the building name or whatever they did at Google
   | otherwise = removeFirstLine pvic


removePrefixZip :: T.Text -> T.Text
removePrefixZip =
   either T.pack id . parseOnly (choice [prefixZip, everythingElse])

   where
      everythingElse :: Parser T.Text
      everythingElse = takeWhile $ const True

      prefixZip :: Parser T.Text
      prefixZip = do
         try $ manyTill digit $ string ", "
         everythingElse


removeFirstLine :: T.Text -> T.Text
removeFirstLine =
   either T.pack id . parseOnly (choice [prefixZip, everythingElse])

   where
      everythingElse :: Parser T.Text
      everythingElse = takeWhile $ const True

      prefixZip :: Parser T.Text
      prefixZip = do
         try $ manyTill anyChar $ string ", "
         everythingElse


test_cleanAddress :: SpecWith ()
test_cleanAddress = describe "cleanAddress" $ do
   it "conventional address" $
      cleanAddress "4035 Lake Boone Trail #109, Raleigh"
      `shouldBe` "4035 Lake Boone Trail #109, Raleigh"
   it "building before street address" $
      cleanAddress "Wells Fargo Capitol Center, 150 Fayetteville Street #2800, Raleigh"
      `shouldBe` "150 Fayetteville Street #2800, Raleigh"
   it "prefixed by zip code" $
      cleanAddress "27604, 3501 Capital Boulevard, Raleigh"
      `shouldBe` "3501 Capital Boulevard, Raleigh"
