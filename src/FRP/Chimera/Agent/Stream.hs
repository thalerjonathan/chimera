{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Agent.Stream 
  (
    dataFlowS
  ) where

import Control.Monad.Reader
import Control.Monad.State
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
-- import FRP.Chimera.Agent.Monad

dataFlowS :: MonadState (AgentOut m o d e) m
          => SF m (AgentData d) ()
dataFlowS = proc _d -> do
  _dt <- arrM_ ask -< ()
  --_ <- arrM (\d -> dataFlowM d) -< d
  returnA -< ()