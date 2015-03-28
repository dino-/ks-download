-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes #-}

module KS.Locate.Places.Geocoding
   ( GeoLatLng (..), forwardLookup )
   where

import Control.Applicative
import Control.Concurrent ( threadDelay )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import KS.Inspection
import KS.Locate.Locate
import KS.Locate.Config
import KS.Log


data GeoLatLng = GeoLatLng Double Double
   deriving Show

instance FromJSON GeoLatLng where
   parseJSON (Object v) = do
      status <- v .: "status"
      when (status /= "OK") $ fail status

      firstResult <- (v .: "results") >>= headE v
      loc <- (firstResult .: "geometry") >>= (.: "location")

      GeoLatLng <$> (loc .: "lat") <*> (loc .: "lng")
   parseJSON o = fail . show $ o


headE :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> [a] -> m a
headE _ (x : _) = return x
headE v []      = fail . show $ v


forwardLookup :: KSDL GeoLatLng
forwardLookup = do
   url <- mkGeocodeUrl
   liftIO $ noticeM lname $ "Geocoding URL: " ++ url

   asks (geocodingApiDelay . getConfig) >>= (liftIO . threadDelay)

   gcJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lname $ "Geocoding result JSON: "
      ++ (BL.unpack gcJSON)

   let parseResult = eitherDecode gcJSON
   either
      (\status -> throwError $ "ERROR Geocoding: " ++ status)
      displayAndReturn parseResult


displayAndReturn :: GeoLatLng -> KSDL GeoLatLng
displayAndReturn location = do
   liftIO $ noticeM lname $ show location
   return location


mkGeocodeUrl :: KSDL String
mkGeocodeUrl = do
   addr' <- asks (addr . inspection . getIdInspection)
   key <- asks (keyString . googleApiKey . getConfig)

   return $ printf "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s" (urlEncode $ unpack addr') key
