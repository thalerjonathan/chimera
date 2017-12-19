module Main 
  ( 
    main

  , runFrSIRStepsAndWriteToFile
  -- , runFrSIRDeltasAndWriteToFile
  , runFrSIRReplicationsAndWriteToFile
  ) where

import Control.Monad.Random
import FRP.Chimera
import FRP.Yampa

import Init
import Model
import Sir

shuffleAgents :: Bool
shuffleAgents = False

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.1

t :: DTime
t = 150

agentCount :: Int
agentCount = 1000

numInfected :: Int
numInfected = 10

main :: IO () 
main = runFrSIRStepsAndWriteToFile

replCfg :: RandomGen g 
        => FrSIREnvironment
        -> FrSIRReplicationConfig g
replCfg env = ReplicationConfig {
    replCfgCount = 4
  , replCfgAgentReplicator = sirAgentDefReplicator env numInfected
  }

runFrSIRStepsAndWriteToFile :: IO ()
runFrSIRStepsAndWriteToFile = do
  params <- initSimulation shuffleAgents (Just rngSeed)
  
  (initAdefs, _) <- createFrSIRNumInfected agentCount numInfected
  
  g <- getStdGen
  let randMs = simulateAggregateTime initAdefs params dt t aggregate
  let dynamics = evalRand randMs g

  let fileName = "frSIRDynamics_" 
                  ++ show agentCount ++ "agents_" 
                  ++ show t ++ "time_"
                  ++ show dt ++ "dt.m"

  writeSirDynamicsFile fileName dt 0 dynamics

  {-
runFrSIRDeltasAndWriteToFile :: IO ()
runFrSIRDeltasAndWriteToFile = do
  params <- initSimulation shuffleAgents (Just rngSeed)
  
  (initAdefs, _) <- createFrSIRNumInfected agentCount numInfected
  
  let deltasBefore = replicate 50 dt
  let deltasZero = replicate 50 0 -- NOTE: this is like halting time, SIR agents won't change as they completely rely on advancing time
  let deltasAfter = replicate 100 dt
  let deltas = deltasZero ++ deltasBefore ++ deltasAfter

  let dynamics = simulateAggregateTimeDeltas initAdefs params deltas aggregate
  let fileName = "frSIRDynamics_" 
                  ++ show agentCount ++ "agents_" 
                  ++ show t ++ "time_"
                  ++ show dt ++ "dt.m"

  writeSirDynamicsFile fileName dt 0 dynamics
-}

runFrSIRReplicationsAndWriteToFile :: IO ()
runFrSIRReplicationsAndWriteToFile = do
  params <- initSimulation shuffleAgents (Just rngSeed)
  
  (initAdefs, env) <- createFrSIRNumInfected agentCount numInfected

  g <- getStdGen

  let rc = replCfg env
  let randMs = runReplicationsWithAggregation initAdefs params dt t rc aggregate
  let replicationDynamics = mapM (\rM -> evalRand rM g) randMs -- TODO: use different RNGs
  let dynamics = sirDynamicsReplMean replicationDynamics
  
  let fileName = "frSIRDynamics_" 
                  ++ show agentCount ++ "agents_" 
                  ++ show t ++ "time_"
                  ++ show dt ++ "dt_"
                  ++ show (replCfgCount rc) ++ "replications.m"

  writeSirDynamicsFile fileName dt (replCfgCount rc) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
aggregate :: (Time, [FrSIRAgentObservable]) -> (Time, Double, Double, Double)
aggregate (t, aobs) = (t, susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . snd) aobs
    infectedCount = fromIntegral $ length $ filter ((Infected==) . snd) aobs
    recoveredCount = fromIntegral $ length $ filter ((Recovered==) . snd) aobs