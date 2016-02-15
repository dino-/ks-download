-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.Locate
   ( Env (..), ErrMsg (..), KSDL, runKSDL
   , tryIO
   , eitherThrowCritical

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
import KS.SourceConfig ( SourceConfig )


data Env = Env
   { getConfig :: Config
   , getSourceConfig :: SourceConfig
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
   \e -> throwError $ ErrMsg CRITICAL $ "CRITICAL IO exception: " ++ (show e)


{- Transform an action of type IO (Either String a) into an action
   in the KSDL monad
-}
eitherThrowCritical :: IO (Either String a) -> KSDL a
eitherThrowCritical action = either
      (\e -> throwError $ ErrMsg CRITICAL $ "CRITICAL IO exception: " ++ e)
      return =<< (liftIO $ action)
