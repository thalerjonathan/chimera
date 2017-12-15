module FRP.Chimera.Simulation.Replication 
  (
    AgentDefReplicator
  , EnvironmentReplicator
  , Replication
  
  , ReplicationConfig (..)

  , defaultEnvReplicator
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

type AgentDefReplicator m o d e = StdGen -> AgentDef m o d e -> (AgentDef m o d e, StdGen)
type EnvironmentReplicator e    = StdGen -> e -> (e, StdGen)
type Replication o e            = [SimulationStepOut o e]
type ReplicationAggregate a     = [a]

data ReplicationConfig m o d e = ReplicationConfig 
  {
    replCfgCount            :: Int
  , replCfgAgentReplicator  :: AgentDefReplicator m o d e
  , replCfgEnvReplicator    :: EnvironmentReplicator e
  }

defaultEnvReplicator :: EnvironmentReplicator e
defaultEnvReplicator rng e = (e, rng)

defaultAgentReplicator :: AgentDefReplicator m o d e
defaultAgentReplicator rng adef = (adef, rng)

runReplicationsWithAggregation :: Monad m
                               => [AgentDef m o d e]
                               -> e
                               -> SimulationParams
                               -> DTime
                               -> Time
                               -> ReplicationConfig m o d e
                               -> AgentObservableAggregator o e a
                               -> [m (ReplicationAggregate a)]
runReplicationsWithAggregation ads e params dt t replCfg aggrFunc = result
  where
    replCount = replCfgCount replCfg
    (replRngs, _) = duplicateRng replCount (simRng params)

    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (runReplicationsWithAggregationAux ads e params aggrFunc replCfg) replRngs

    runReplicationsWithAggregationAux :: Monad m
                                      => [AgentDef m o d e]
                                      -> e
                                      -> SimulationParams
                                      -> AgentObservableAggregator o e a
                                      -> ReplicationConfig m o d e
                                      -> StdGen 
                                      -> m (ReplicationAggregate a)
    runReplicationsWithAggregationAux ads e params aggrFunc replCfg replRng = 
        simulateAggregateTime ads' e (params { simRng = replRng' }) dt t aggrFunc
      where
        (ads', replRng') = foldr (adsFoldAux replCfg) ([], replRng) ads

runReplications :: Monad m
                => [AgentDef m o d e]
                -> e
                -> SimulationParams
                -> DTime
                -> Time
                -> ReplicationConfig m o d e
                -> [m (Replication o e)]
runReplications ads e params dt t replCfg = result
  where
    replCount = replCfgCount replCfg
    (replRngs, _) = duplicateRng replCount (simRng params)

    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (runReplicationsAux ads e params replCfg) replRngs

    runReplicationsAux :: Monad m
                       => [AgentDef m o d e]
                       -> e
                       -> SimulationParams
                       -> ReplicationConfig m o d e
                       -> StdGen 
                       -> m (Replication o e)
    runReplicationsAux ads e params replCfg replRng = 
        simulateTime ads' e (params { simRng = replRng' }) dt t
      where
        (ads', replRng') = foldr (adsFoldAux replCfg) ([], replRng) ads

adsFoldAux :: ReplicationConfig m o d e 
           -> AgentDef m o d e 
           -> ([AgentDef m o d e], StdGen) 
           -> ([AgentDef m o d e], StdGen)
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