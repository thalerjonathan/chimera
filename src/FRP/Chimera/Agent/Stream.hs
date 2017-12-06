{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Agent.Stream 
  (
    dataFlowS
  , agentObservableS
  ) where

import Control.Monad.State
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Monad

dataFlowS :: Monad m => SF (StateT (AgentOut m o d e) m) (AgentData d) ()
dataFlowS = arrM (lift . dataFlowM)

-- NOTE: assuming that state isJust
agentObservableS :: Monad m => SF (StateT (AgentOut m o d e) m) a o
agentObservableS = arrM_ (do
  o <- lift agentObservableM -- TODO: can't we refactor this in a point-free style?
  return o)