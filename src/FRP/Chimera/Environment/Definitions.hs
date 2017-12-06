{-# LANGUAGE Arrows #-}
module FRP.Chimera.Environment.Definitions 
  (
    Environment
  , EnvironmentMonadic

  , EnvironmentFolding

  , environmentMonadic
  ) where

import Control.Monad.State

import FRP.BearRiver

type Environment m e                = SF (StateT e m) () ()
type EnvironmentMonadic m e         = Double -> StateT e m ()

type EnvironmentFolding e           = [e] -> e

environmentMonadic :: Monad m => EnvironmentMonadic m e -> Environment m e
environmentMonadic f = proc _ -> do
  t   <- time             -< ()
  _   <- arrM (lift . f)  -< t
  returnA -< ()