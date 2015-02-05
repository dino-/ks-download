-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ks.Locate.Locate
   ( Env (..), Ksdl, runKsdl

   -- Re-exporting
   , asks, liftIO, local, throwError, when
   )
   where

import Control.Monad.Reader
import Control.Monad.Error

import Ks.Inspection
import Ks.Locate.Config


data Env = Env
   { getConfig :: Config
   , getIdInspection :: IdInspection
   }

type Ksdl a = ReaderT Env (ErrorT String IO) a

runKsdl :: Env -> Ksdl a -> IO (Either String a)
runKsdl env ev = runErrorT (runReaderT ev env)
