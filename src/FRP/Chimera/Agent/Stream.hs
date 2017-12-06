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
-- import FRP.Chimera.Agent.Monad

dataFlowS :: Monad m
          => SF (StateT (AgentOut m o d e) m) (AgentData d) ()
dataFlowS = proc d -> do
  _ <- arrM (\d -> do
    ao <- get
    let ao' = dataFlow d ao
    put ao'
    return ()) -< d
  returnA -< ()

-- NOTE: assuming that state isJust
agentObservableS :: Monad m
                 => SF (StateT (AgentOut m o d e) m) a o
agentObservableS = proc _ -> do
  o <- arrM (\_ -> do
      ao <- get
      return $ agentObservable ao) -< ()
  returnA -< o