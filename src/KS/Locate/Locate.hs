-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.Locate
   ( Env (..), KSDL, runKSDL
   , tryIO

   -- Re-exporting
   , asks, liftIO, local, throwError, when
   )
   where

import Control.Monad.Catch ( catchAll )
import Control.Monad.Reader
import Control.Monad.Except

import KS.Data.Inspection
import KS.Locate.Config


data Env = Env
   { getConfig :: Config
   , getInspection :: Inspection
   }

type KSDL a = ReaderT Env (ExceptT String IO) a

runKSDL :: Env -> KSDL a -> IO (Either String a)
runKSDL env ev = runExceptT (runReaderT ev env)


{- Catch IOError and throw in the KSDL monad

   catchAll comes from the `exceptions` package
-}
tryIO :: IO a -> KSDL a
tryIO act = catchAll (liftIO act) $ throwError . show
