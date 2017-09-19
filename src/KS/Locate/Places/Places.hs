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
   ( Distance (..)
   , coordsToPlaces
   , computeDistance
   )
   where

import Data.Aeson ( FromJSON, Value (Object)
   , (.:), (.:?), (.!=), eitherDecode, parseJSON )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Maybe ( catMaybes )
import Data.Text ( Text, intercalate, unpack )
import Datum ( etrf89Datum )
import GHC.Generics ( Generic )
import LatLng ( LatLng (..), distance )
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import KS.Data.Place ( GeoPoint (..), Place (..) )
import qualified KS.Data.Place as P
import KS.Locate.Config ( Config (googleApiKey), keyString )
import KS.Locate.Locate
   ( Env (getConfig, getSourceConfig)
   , ErrMsg (..), KSDL
   , asks, eitherThrowCritical, liftIO,
   throwError, when
   )
import KS.Locate.Places.Geocoding ( GeoLatLng (..) )
import KS.Locate.Places.NameWords ( toList )
import KS.Log ( Priority (ERROR), debugM, errorM, lname, noticeM )
import KS.SourceConfig ( SourceConfig (placesTypes) )
import KS.Util ( withRetry )


data RawPlace = RawPlace
   { name :: Text
   , vicinity :: Text
   , location :: GeoPoint
   , types :: [String]
   , place_id :: Text
   , closed :: Bool
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
         <*> o .:? "permanently_closed" .!= False
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


newtype Distance = Distance Double
   deriving (Eq, Ord)

instance Show Distance where
   show (Distance distanceInKm) = printf "%5.4f km" distanceInKm


coordsToPlaces :: GeoLatLng -> KSDL [(Distance, Place)]
coordsToPlaces coords = do
   url <- mkPlacesUrl coords
   liftIO $ noticeM lname $ "Places URL: " ++ url

   plJSON <- eitherThrowCritical $ withRetry 5 3 (simpleHttp url) (errorM lname)

   liftIO $ debugM lname $ "Places result JSON: "
      ++ (BL.unpack plJSON)

   let parseResult = eitherDecode plJSON
   either
      (\status -> throwError $ ErrMsg ERROR $ "ERROR Places API: " ++ status)
      displayAndReturn parseResult

   where
      displayAndReturn :: Places -> KSDL [(Distance, Place)]
      displayAndReturn (Places rps) = do
         let ps = L.map (computeDistance coords) . catMaybes . L.map convert $ rps
         liftIO $ do
            noticeM lname "Places returned:"
            mapM_ (noticeM lname . show) ps
         return ps


convert :: RawPlace -> Maybe Place
convert (RawPlace _ _ _ _ _   True) = Nothing
convert (RawPlace n v l t pid _   ) = Just $ Place n v l t pid


computeDistance :: GeoLatLng -> Place -> (Distance, Place)
computeDistance (GeoLatLng inspLat inspLng) pl =
   let inspectionLoc = LatLng inspLat inspLng 0.0 etrf89Datum
       restaurantLoc = LatLng (lat . P.location $ pl) (lng . P.location $ pl) 0.0 etrf89Datum
       dist' = distance inspectionLoc restaurantLoc
       dist = if isNaN dist'
         then Distance 0.0
         else Distance dist'
   in (dist, pl)


mkPlacesUrl :: GeoLatLng -> KSDL String
mkPlacesUrl (GeoLatLng lat' lng') = do
   key <- asks (keyString . googleApiKey . getConfig)

   nameList <- toList
   liftIO $ noticeM lname $ "Places name words list: "
      ++ (show nameList)

   let nameWordsParam = urlEncode . unpack . intercalate " " $ nameList

   searchTypes <-
      L.intercalate "|" `fmap` asks (placesTypes . getSourceConfig)

   return $ printf "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&rankby=distance&keyword=%s&types=%s" key lat' lng' nameWordsParam searchTypes
