-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Util
   ( withRetry
   )
   where

import Control.Concurrent ( threadDelay )
import Control.Monad.Catch ( catchAll )


withRetry :: Int -> Int -> IO a -> (String -> IO ()) -> IO (Either String a)
withRetry tryNumber delay action logF = withRetry' tryNumber where
   withRetry' 0          = return . Left $ "Failed because retries are exhausted"
   withRetry' tryNumber' = do
      response <- catchAll (Right <$> action) (return . Left . show)
      case response of
         r@(Right _) -> return r
         Left e      -> do
            logF $ "Failed, retrying. Error: " ++ e
            threadDelay $ delay * 1000 * 1000
            withRetry' $ tryNumber' - 1
