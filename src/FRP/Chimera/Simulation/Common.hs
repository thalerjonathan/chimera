module FRP.Chimera.Simulation.Common 
  (
    AgentObservable
  , SimulationStepOut

  --, shuffleAgents
  , observableAgents
  ) where

import Data.Maybe

import FRP.BearRiver

import FRP.Chimera.Agent.Interface
--import FRP.Chimera.Random.Pure
--import FRP.Chimera.Simulation.Init

type AgentObservable o   = (AgentId, o)
type SimulationStepOut o = (Time, [AgentObservable o])

{-
shuffleAgents :: SimulationParams 
              -> [a]
              -> [b]
              -> (SimulationParams, [a], [b])
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
-}

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