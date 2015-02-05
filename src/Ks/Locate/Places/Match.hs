-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Ks.Locate.Places.Match
   ( Match, match )
   where

import Data.Attoparsec.Text hiding ( count, match )
import Data.Char ( isDigit )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import Prelude hiding ( takeWhile )

import Ks.Locate.Locate
import qualified Ks.Inspection as I
import Ks.Log
import qualified Ks.Locate.Places.Place as P


type Match = (I.IdInspection, P.Place)

type MatchInternal = (Bool, Match)


match :: [P.Place] -> Ksdl Match
match ps = do
   idInsp <- asks getIdInspection
   let mis = map (combine idInsp) ps
   let count = (sum . map bToI $ mis) :: Int

   when (count == 0) $ do
      throwError "ERROR Match: No Places result matches"

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
      combine :: I.IdInspection -> P.Place -> MatchInternal
      combine idInsp pl = (matched, (idInsp, pl { P.vicinity = newPvic }))
         where
            (matched, newPvic) =
               isMatch (I.addr . I.inspection $ idInsp) (P.vicinity pl)

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
isMatch iaddr pvic = (prefix iaddr == prefix newPvic, newPvic)
   where
      newPvic = removePrefixZip pvic
      prefix = T.takeWhile isDigit


{- We get these ridiculous addresses from Google Places where they've
   clearly mistakenly put the zip code up front. They look like this:

      "27603, 7900 Fayetteville Road, Raleigh"

   This parser returns the address string with that zip code,
   comma and space removed.
-}
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
