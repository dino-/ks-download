-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes #-}

module KS.Locate.Places.Geocoding
   ( GeoPoint (..), forwardLookup )
   where

import Control.Concurrent ( threadDelay )
import Data.Aeson ( FromJSON, Value (Object), (.:), eitherDecode,
   parseJSON )
import Data.String.Conv ( toS )
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import KS.Data.Inspection ( addr )
import KS.Data.Place ( GeoPoint (GeoPoint), lat, lng )
import KS.Locate.Locate ( Env (..), ErrMsg (..), KSDL, asks,
   eitherThrowCritical, liftIO, throwError, when )
import KS.Locate.Config ( Config (geocodingApiDelay, googleApiKey),
   keyString )
import KS.Log ( Priority (ERROR), debugM, errorM, lname, noticeM )
import KS.Util ( Seconds (..), TryCount (Remaining), withRetry )


newtype GCPoint = GCPoint { unwrapGCPoint :: GeoPoint }

instance FromJSON GCPoint where
   parseJSON (Object v) = do
      status <- v .: "status"
      when (status /= "OK") $ fail status

      firstResult <- (v .: "results") >>= headE v
      loc <- (firstResult .: "geometry") >>= (.: "location")

      GCPoint <$> ( GeoPoint <$> (loc .: "lat") <*> (loc .: "lng") )
   parseJSON o = fail . show $ o


headE :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> [a] -> m a
headE _ (x : _) = return x
headE v []      = fail . show $ v


forwardLookup :: KSDL GeoPoint
forwardLookup = do
   url <- mkGeocodeUrl
   liftIO $ noticeM lname $ "Geocoding URL: " ++ url

   asks (geocodingApiDelay . getConfig) >>= (liftIO . threadDelay)

   gcJSON <- eitherThrowCritical $ withRetry (Remaining 5) (Seconds 3) (simpleHttp url) (errorM lname)

   liftIO $ debugM lname $ "Geocoding result JSON: " ++ (toS gcJSON)

   let parseResult = unwrapGCPoint <$> eitherDecode gcJSON
   either
      (\status -> throwError $ ErrMsg ERROR $ "ERROR Geocoding: " ++ status)
      displayAndReturn parseResult


displayAndReturn :: GeoPoint -> KSDL GeoPoint
displayAndReturn location = do
   liftIO $ noticeM lname $ show location
   return location


mkGeocodeUrl :: KSDL String
mkGeocodeUrl = do
   addr' <- asks (addr . getInspection)
   key <- asks (keyString . googleApiKey . getConfig)

   return $ printf "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s" (urlEncode $ toS addr') key
