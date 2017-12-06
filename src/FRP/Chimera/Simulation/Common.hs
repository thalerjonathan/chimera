{-# LANGUAGE Arrows #-}
module FRP.Chimera.Simulation.Common 
  (
    SimulationStepOut

  , runEnv
  , shuffleAgents
  , newAgentIn
  , observableAgents
  ) where

import Data.Maybe

import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Init
import FRP.Chimera.Random.Pure

type AgentObservable o      = (AgentId, o)
type SimulationStepOut s e  = (Time, [AgentObservable s], e)

runEnv :: Monad m => SF m (SimulationParams e, e) (SimulationParams e, e)
runEnv = proc (params, e) -> do
  -- TODO: repair
  {-
  let mayEnvBeh = simEnvBehaviour params
  maybe (e, params) (runEnvAux params e) mayEnvBeh
  (envBeh', e') = runAndFreezeSF envBeh e dt
  params' = params { simEnvBehaviour = Just envBeh' }
  -}
  returnA -< (params, e)

shuffleAgents :: SimulationParams e 
              -> [a] 
              -> [b] 
              -> (SimulationParams e, [a], [b])
shuffleAgents params as bs 
    | doShuffle = (params', as', bs')
    | otherwise = (params, as, bs)
  where
    doShuffle = simShuffleAgents params
    g = simRng params 

    sfsIns = zip as bs
    (shuffledSfsIns, g') = fisherYatesShuffle g sfsIns

    params' = params { simRng = g' }
    (as', bs') = unzip shuffledSfsIns

newAgentIn :: AgentIn s m e -> AgentIn s m e
newAgentIn oldIn  = 
  oldIn { 
    aiStart = NoEvent
  , aiData  = []
  }

observableAgents :: [AgentId] 
                 -> [AgentOut m o d e] 
                 -> [AgentObservable o]
observableAgents ais aos = foldl observableAgents [] (zip ais aos)
  where
    observableAgents :: [AgentObservable o] 
                     -> (AgentId, AgentOut m o d e) 
                     -> [AgentObservable o] 
    observableAgents acc (aid, ao) 
        | isJust mayObs = (aid, fromJust mayObs) : acc
        | otherwise = acc
      where
        mayObs = aoObservable ao