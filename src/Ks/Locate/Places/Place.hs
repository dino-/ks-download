-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Ks.Locate.Places.Place
   ( Place (..)
   , coordsToPlaces )
   where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Text
import GHC.Generics ( Generic )
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import Ks.Locate.Config
import Ks.Locate.Locate
import Ks.Locate.Places.Geocoding ( GeoLatLng (..) )
import Ks.Locate.Places.NameWords ( toList )
import Ks.Log


data PlLatLng = PlLatLng
   { lat :: Double
   , lng :: Double
   }
   deriving (Generic, Show)

instance FromJSON PlLatLng
instance ToJSON PlLatLng


data Place = Place
   { name :: Text
   , vicinity :: Text
   , location :: PlLatLng
   , types :: [String]
   , place_id :: Text
   }
   deriving Show

instance FromJSON Place where
   parseJSON (Object o) = do
      l <- (o .: "geometry") >>= (.: "location")
      Place
         <$> o .: "name"
         <*> o .: "vicinity"
         <*> (PlLatLng <$> (l .: "lat") <*> (l .: "lng"))
         <*> o .: "types"
         <*> o .: "place_id"
   parseJSON o = fail . show $ o


newtype Places = Places [Place]
   deriving Show

instance FromJSON Places where
   parseJSON (Object v) = do
      status <- v .: "status"
      when (status /= "OK") $ fail status

      rs <- v .: "results"
      when (L.null rs) $ fail . show $ v

      return $ Places rs
   parseJSON o = fail . show $ o


coordsToPlaces :: GeoLatLng -> Ksdl [Place]
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


displayAndReturn :: Places -> Ksdl [Place]
displayAndReturn (Places ps) = do
   liftIO $ do
      noticeM lname "Places returned:"
      mapM_ (noticeM lname . show) ps
   return ps


mkPlacesUrl :: GeoLatLng -> Ksdl String
mkPlacesUrl (GeoLatLng lat' lng') = do
   key <- asks (keyString . googleApiKey . getConfig)

   nameWords <- toList
   liftIO $ noticeM lname $ "Places name words list: "
      ++ (show nameWords)

   let nameList = urlEncode $ unpack $ intercalate " " $ nameWords

   searchTypes <-
      L.intercalate "|" `fmap` asks (placesTypes . getConfig)

   return $ printf "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&rankby=distance&name=%s&types=%s" key lat' lng' nameList searchTypes
