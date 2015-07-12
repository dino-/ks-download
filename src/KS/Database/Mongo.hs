-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Database.Mongo
   ( parseLastError )
   where

import Data.Bson
import qualified Data.Text as T
import Text.Printf ( printf )


parseLastError :: Document -> String
parseLastError errdoc = either id id $ lastErrStatus >> atE "err"
   where
      lastErrStatus :: Either String String
      lastErrStatus = case at "ok" errdoc of
         1.0   -> Right "getLastError successful"
         c     -> Left $ printf "getLastError FAILED: ok: %f" (c :: Double)

      atE :: T.Text -> Either String String
      atE key = case at key errdoc of
         Just msg -> Left $ printf "insertion FAILED: %s" (T.unpack msg)
         Nothing  -> Right "insertion successful"
