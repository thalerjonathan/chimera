module FRP.Chimera.Simulation.Replication 
  (
    AgentDefReplicator
  , Replication
  
  , ReplicationConfig (..)

  , defaultAgentReplicator

  , runReplications
  , runReplicationsWithAggregation
  ) where

import Control.Monad.Random
import Control.Parallel.Strategies

import FRP.Yampa

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Common
import FRP.Chimera.Simulation.Simulation
import FRP.Chimera.Simulation.Init

type AgentDefReplicator m o d   = StdGen -> AgentDef m o d -> (AgentDef m o d, StdGen)
type Replication o              = [SimulationStepOut o]
type ReplicationAggregate a     = [a]

data ReplicationConfig m o d = ReplicationConfig 
  {
    replCfgCount            :: Int
  , replCfgAgentReplicator  :: AgentDefReplicator m o d
  }

defaultAgentReplicator :: AgentDefReplicator m o d
defaultAgentReplicator rng adef = (adef, rng)

runReplicationsWithAggregation :: Monad m
                               => [AgentDef m o d]
                               -> SimulationParams
                               -> DTime
                               -> Time
                               -> ReplicationConfig m o d
                               -> AgentObservableAggregator o a
                               -> [m (ReplicationAggregate a)]
runReplicationsWithAggregation ads params dt t replCfg aggrFunc = result
  where
    replCount = replCfgCount replCfg
    (replRngs, _) = duplicateRng replCount (simRng params)

    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (runReplicationsWithAggregationAux ads params aggrFunc replCfg) replRngs

    runReplicationsWithAggregationAux :: Monad m
                                      => [AgentDef m o d]
                                      -> SimulationParams
                                      -> AgentObservableAggregator o a
                                      -> ReplicationConfig m o d
                                      -> StdGen 
                                      -> m (ReplicationAggregate a)
    runReplicationsWithAggregationAux ads params aggrFunc replCfg replRng = 
        simulateAggregateTime ads' (params { simRng = replRng' }) dt t aggrFunc
      where
        (ads', replRng') = foldr (adsFoldAux replCfg) ([], replRng) ads

runReplications :: Monad m
                => [AgentDef m o d]
                -> SimulationParams
                -> DTime
                -> Time
                -> ReplicationConfig m o d
                -> [m (Replication o)]
runReplications ads params dt t replCfg = result
  where
    replCount = replCfgCount replCfg
    (replRngs, _) = duplicateRng replCount (simRng params)

    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (runReplicationsAux ads params replCfg) replRngs

    runReplicationsAux :: Monad m
                       => [AgentDef m o d]
                       -> SimulationParams
                       -> ReplicationConfig m o d
                       -> StdGen 
                       -> m (Replication o)
    runReplicationsAux ads params replCfg replRng = 
        simulateTime ads' (params { simRng = replRng' }) dt t
      where
        (ads', replRng') = foldr (adsFoldAux replCfg) ([], replRng) ads

adsFoldAux :: ReplicationConfig m o d 
           -> AgentDef m o d 
           -> ([AgentDef m o d], StdGen) 
           -> ([AgentDef m o d], StdGen)
adsFoldAux cfg ad (adsAcc, rngAcc) = (ad' : adsAcc, rngAcc')
  where
    (ad', rngAcc') = (replCfgAgentReplicator cfg) rngAcc ad

duplicateRng :: Int -> StdGen -> ([StdGen], StdGen)
duplicateRng n g = duplicateRngAux n g []
  where
    duplicateRngAux :: Int -> StdGen -> [StdGen] -> ([StdGen], StdGen)
    duplicateRngAux 0 g acc = (acc, g)
    duplicateRngAux n g acc = duplicateRngAux (n-1) g' (g'' : acc)
      where
        (g', g'') = split g