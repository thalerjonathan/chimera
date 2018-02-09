{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Agent.Stream 
  (
    dataFlowS
  , dataFlowsS
  
  , agentObservableS
  , setAgentObservableS
  ) where

import Control.Monad.State.Strict
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Monad

dataFlowS :: Monad m
          => SF (StateT (AgentOut m o d) m) (DataFlow d) ()
dataFlowS = arrM (lift . dataFlowM)

dataFlowsS :: Monad m
           => SF (StateT (AgentOut m o d) m) [DataFlow d] ()
dataFlowsS = arrM (lift . dataFlowsM)

-- NOTE: assuming that state isJust
agentObservableS :: Monad m => SF (StateT (AgentOut m o d) m) a o
agentObservableS = arrM_ (lift agentObservableM)

setAgentObservableS :: Monad m => SF (StateT (AgentOut m o d) m) o ()
setAgentObservableS = arrM (lift . setAgentObservableM)