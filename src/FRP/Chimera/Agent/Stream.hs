{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Agent.Stream 
  (
    dataFlowS

  , agentObservableS
  , setAgentObservableS
  ) where

import Control.Monad.State.Strict
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Monad

dataFlowS :: Monad m => SF (StateT (AgentOut m o d e) m) (AgentData d) ()
dataFlowS = arrM (lift . dataFlowM)

-- NOTE: assuming that state isJust
agentObservableS :: Monad m => SF (StateT (AgentOut m o d e) m) a o
agentObservableS = arrM_ (lift agentObservableM)

setAgentObservableS :: Monad m => SF (StateT (AgentOut m o d e) m) o ()
setAgentObservableS = arrM (lift . setAgentObservableM)