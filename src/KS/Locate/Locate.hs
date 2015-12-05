-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.Locate
   ( Env (..), ErrMsg (..), KSDL, runKSDL
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
import KS.Log ( Priority (..) )


data Env = Env
   { getConfig :: Config
   , getInspection :: Inspection
   }

data ErrMsg = ErrMsg Priority String
   deriving (Eq, Show)

type KSDL a = ReaderT Env (ExceptT ErrMsg IO) a

runKSDL :: Env -> KSDL a -> IO (Either ErrMsg a)
runKSDL env ev = runExceptT (runReaderT ev env)


{- Catch IOError and throw in the KSDL monad

   catchAll comes from the `exceptions` package
-}
tryIO :: IO a -> KSDL a
tryIO act = catchAll (liftIO act) $
   \e -> throwError $ ErrMsg CRITICAL $ show e
