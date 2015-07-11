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

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Text
import GHC.Generics ( Generic )
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import KS.Data.Place
import KS.Locate.Config
import KS.Locate.Locate
import KS.Locate.Places.Geocoding ( GeoLatLng (..) )
import KS.Locate.Places.NameWords ( toList )
import KS.Log


data RawPlace = RawPlace
   { name :: Text
   , vicinity :: Text
   , location :: PlLatLng
   , types :: [String]
   , place_id :: Text
   }
   deriving Generic

instance FromJSON RawPlace where
   parseJSON (Object o) = do
      l <- (o .: "geometry") >>= (.: "location")
      RawPlace
         <$> o .: "name"
         <*> o .: "vicinity"
         <*> (PlLatLng <$> (l .: "lat") <*> (l .: "lng"))
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

   plJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lname $ "Places result JSON: "
      ++ (BL.unpack plJSON)

   let parseResult = eitherDecode plJSON
   either
      (\status -> throwError $ "ERROR Places API: " ++ status)
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
      L.intercalate "|" `fmap` asks (placesTypes . getConfig)

   return $ printf "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&rankby=distance&name=%s&types=%s" key lat' lng' nameList searchTypes
