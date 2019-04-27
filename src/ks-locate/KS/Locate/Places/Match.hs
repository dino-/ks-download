-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module KS.Locate.Places.Match
   ( Match, match
   , test_cleanAddress
   )
   where

import Control.Arrow ( second )
import Control.Monad ( when )
import Data.Attoparsec.Text hiding ( match )
import Data.Char ( isDigit )
import qualified Data.Text as T
import Prelude hiding ( takeWhile )
import Test.Hspec ( SpecWith, describe, it, shouldBe )

import KS.Locate.Locate ( ErrMsg (..), KSDL, asks, getInspection,
   liftIO, throwError )
import KS.Locate.Places.Places ( Distance (..) )
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P
import KS.Log ( Priority (ERROR), lname, noticeM, warningM )


type Match = (I.Inspection, P.Place)


match :: [(Distance, P.Place)] -> KSDL Match
match dps = do
   insp <- asks getInspection
   let cleanedDps = map (second cleanPlaceAddress) dps

   finalMatches <- do
      -- First, let's try by address
      let matchedByAddress = filter (isAddrMatch insp . snd) cleanedDps

      when (null matchedByAddress) $ liftIO $
         noticeM lname "No matches found by address"

      -- Next, let's try using distance
      let matchedByDistance = filter (isVeryClose . fst) cleanedDps

      when (null matchedByDistance) $ liftIO $
         noticeM lname "No matches found by distance"

      -- Evaluate to whichever of these was successful first
      return $ matchedByAddress <++ matchedByDistance

   when (null finalMatches) $ do
      throwError $ ErrMsg ERROR "ERROR Match: No Places result matches"

   liftIO $ do
      noticeM lname "Matches:"
      mapM_ (noticeM lname) $ map (fmtMatched . snd) finalMatches

   when (length finalMatches > 1) $ liftIO $
      warningM lname "WARNING Match: More than one Places result matched"

   return (insp, (snd . head $ finalMatches))

   where
      cleanPlaceAddress :: P.Place -> P.Place
      cleanPlaceAddress oldPlace = oldPlace { P.vicinity = newPvic }
         where newPvic = cleanAddress . P.vicinity $ oldPlace


      {- Determine if two addresses are a "match" based on the
         beginning digits. Given how close we get with Google Place
         search, this gets us the rest of the way to disambiguate
         the hits.
      -}
      isAddrMatch :: I.Inspection -> P.Place -> Bool
      isAddrMatch insp pl =
         (not . T.null . I.addr $ insp) &&
         (not . T.null . P.vicinity $ pl) &&
         ((prefix . I.addr $ insp) == (prefix . P.vicinity $ pl))

         where prefix = T.takeWhile isDigit


      isVeryClose :: Distance -> Bool
      isVeryClose dist = dist <= (Distance 0.1372)


      fmtMatched :: P.Place -> String
      fmtMatched pl = T.unpack . T.concat
         $ [ P.name pl, T.pack " | ", P.vicinity pl ]


      [] <++ xs = xs
      xs <++ _  = xs


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
