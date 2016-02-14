-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}

{-| This module is used for parsing return data from the Google
    Places API results.

    To achieve that goal, it contains a custom datatype and JSON
    instance, RawPlace, that's not used anywhere else. The reason
    for this custom parsing is that the Places API returns a lot
    of data we have no interest in. The custom instancing allows
    us to discard this unused information.
-}

module KS.Locate.Places.Places
   ( coordsToPlaces )
   where

import           Data.Aeson ( FromJSON, Value (Object), (.:), eitherDecode,
                  parseJSON )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import           Data.Text ( Text, intercalate, unpack )
import           GHC.Generics ( Generic )
import           Network.HTTP ( urlEncode )
import           Network.HTTP.Conduit ( simpleHttp )
import           Text.Printf ( printf )

import           KS.Data.Place ( GeoPoint (..), Place (..) )
import           KS.Locate.Config ( Config (googleApiKey), keyString )
import           KS.Locate.Locate
                  ( Env (getConfig, getSourceConfig)
                  , ErrMsg (..), KSDL
                  , asks, eitherThrowCritical, liftIO,
                  throwError, when
                  )
import           KS.Locate.Places.Geocoding ( GeoLatLng (..) )
import           KS.Locate.Places.NameWords ( toList )
import           KS.Locate.SourceConfig ( SourceConfig (placesTypes) )
import           KS.Log ( Priority (ERROR), debugM, errorM, lname, noticeM )
import           KS.Util ( withRetry )


data RawPlace = RawPlace
   { name :: Text
   , vicinity :: Text
   , location :: GeoPoint
   , types :: [String]
   , place_id :: Text
   }
   deriving Generic

instance FromJSON RawPlace where
   parseJSON (Object o) = do
      l <- (o .: "geometry") >>= (.: "location")
      lng' <- l .: "lng"
      lat' <- l .: "lat"
      RawPlace
         <$> o .: "name"
         <*> o .: "vicinity"
         <*> (return $ GeoPoint lat' lng')
         <*> o .: "types"
         <*> o .: "place_id"
   parseJSON o = fail . show $ o


newtype Places = Places [RawPlace]

instance FromJSON Places where
   parseJSON (Object v) = do
      status <- v .: "status"
      when (status /= "OK") $ fail status

      rs <- v .: "results"
      when (L.null rs) $ fail . show $ v

      return $ Places rs
   parseJSON o = fail . show $ o


coordsToPlaces :: GeoLatLng -> KSDL [Place]
coordsToPlaces coords = do
   url <- mkPlacesUrl coords
   liftIO $ noticeM lname $ "Places URL: " ++ url

   plJSON <- eitherThrowCritical $ withRetry 3 2 (simpleHttp url) (errorM lname)

   liftIO $ debugM lname $ "Places result JSON: "
      ++ (BL.unpack plJSON)

   let parseResult = eitherDecode plJSON
   either
      (\status -> throwError $ ErrMsg ERROR $ "ERROR Places API: " ++ status)
      displayAndReturn parseResult


convert :: RawPlace -> Place
convert (RawPlace n v l t pid) = Place n v l t pid


displayAndReturn :: Places -> KSDL [Place]
displayAndReturn (Places rps) = do
   let ps = L.map convert rps
   liftIO $ do
      noticeM lname "Places returned:"
      mapM_ (noticeM lname . show) ps
   return ps


mkPlacesUrl :: GeoLatLng -> KSDL String
mkPlacesUrl (GeoLatLng lat' lng') = do
   key <- asks (keyString . googleApiKey . getConfig)

   nameWords <- toList
   liftIO $ noticeM lname $ "Places name words list: "
      ++ (show nameWords)

   let nameList = urlEncode $ unpack $ intercalate " " $ nameWords

   searchTypes <-
      L.intercalate "|" `fmap` asks (placesTypes . getSourceConfig)

   return $ printf "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&rankby=distance&name=%s&types=%s" key lat' lng' nameList searchTypes
