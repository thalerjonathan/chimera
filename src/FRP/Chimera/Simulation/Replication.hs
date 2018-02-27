module FRP.Chimera.Simulation.Replication 
  (
    AgentDefReplicator
  , Replication
  
  , ReplicationConfig (..)

  , defaultAgentReplicator

  , runReplications
  , runReplicationsWithAggregation

  , duplicateRng
  ) where

import Control.Monad.Random
import Control.Parallel.Strategies

import FRP.Yampa

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Common
import FRP.Chimera.Simulation.Simulation

type AgentDefReplicator m o d e = AgentDef m o d e -> AgentDef m o d e
type Replication o              = [SimulationStepOut o]
type ReplicationAggregate a     = [a]

data ReplicationConfig m o d e = ReplicationConfig 
  { replCfgCount            :: Int
  , replCfgAgentReplicator  :: AgentDefReplicator m o d e
  }

defaultAgentReplicator :: AgentDefReplicator m o d e
defaultAgentReplicator = id

runReplicationsWithAggregation :: Monad m
                               => [AgentDef m o d e]
                               -> DTime
                               -> Time
                               -> ReplicationConfig m o d e
                               -> AgentObservableAggregator o a
                               -> [m (ReplicationAggregate a)]
runReplicationsWithAggregation ads _dt _t replCfg aggrFunc = result
  where
    replCount = replCfgCount replCfg
    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (const $ runReplicationsWithAggregationAux ads aggrFunc replCfg) [1..replCount]

    runReplicationsWithAggregationAux :: Monad m
                                      => [AgentDef m o d e]
                                      -> AgentObservableAggregator o a
                                      -> ReplicationConfig m o d e
                                      -> m (ReplicationAggregate a)
    runReplicationsWithAggregationAux ads _aggrFunc replCfg = undefined
        -- simulateAggregateTime ads' dt t aggrFunc
      where
        _ads' = foldr (adsFoldAux replCfg) [] ads

runReplications :: Monad m
                => [AgentDef m o d e]
                -> DTime
                -> Time
                -> ReplicationConfig m o d e
                -> [m (Replication o)]
runReplications ads dt t replCfg = result
  where
    replCount = replCfgCount replCfg
    
    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (const $ runReplicationsAux ads replCfg) [1..replCount]

    runReplicationsAux :: Monad m
                       => [AgentDef m o d e]
                       -> ReplicationConfig m o d e
                       -> m (Replication o)
    runReplicationsAux ads replCfg = 
        simulateTime ads' dt t
      where
        ads' = foldr (adsFoldAux replCfg) [] ads

adsFoldAux :: ReplicationConfig m o d e
           -> AgentDef m o d e
           -> [AgentDef m o d e]
           -> [AgentDef m o d e]
adsFoldAux cfg ad adsAcc = ad' : adsAcc
  where
    ad' = replCfgAgentReplicator cfg ad

duplicateRng :: Int -> StdGen -> ([StdGen], StdGen)
duplicateRng n g = duplicateRngAux n g []
  where
    duplicateRngAux :: Int -> StdGen -> [StdGen] -> ([StdGen], StdGen)
    duplicateRngAux 0 g acc = (acc, g)
    duplicateRngAux n g acc = duplicateRngAux (n-1) g' (g'' : acc)
      where
        (g', g'') = split g