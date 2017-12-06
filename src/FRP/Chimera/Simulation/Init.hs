module FRP.Chimera.Simulation.Init 
  (
    SimulationParams (..)
  , UpdateStrategy (..)

  , initRng
  , initSimulation
  , initSimNoEnv
  , newAgentId
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad.Random

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Environment.Definitions
import FRP.Chimera.Simulation.Internal

data UpdateStrategy      = Sequential 
                         | Parallel deriving (Eq, Show)

data SimulationParams m e = SimulationParams 
  {
    simStrategy       :: UpdateStrategy
  , simEnv            :: Maybe (Environment m e)
  , simEnvFold        :: Maybe (EnvironmentFolding e)
  , simShuffleAgents  :: Bool
  , simRng            :: StdGen
  , simIdGen          :: TVar Int
  }

initSimulation :: Monad m
               => UpdateStrategy
               -> Maybe (Environment m e)
               -> Maybe (EnvironmentFolding e)
               -> Bool
               -> Maybe Int
               -> IO (SimulationParams m e)
initSimulation updtStrat beh foldEnvFun shuffAs rngSeed = do
  initRng rngSeed

  rng <- getSplit
  agentIdVar <- newTVarIO 0

  return SimulationParams {
      simStrategy = updtStrat
    , simEnv = beh
    , simEnvFold = foldEnvFun
    , simShuffleAgents = shuffAs
    , simRng = rng
    , simIdGen = agentIdVar
    }

initSimNoEnv :: Monad m
             => UpdateStrategy
             -> Bool
             -> Maybe Int
             -> IO (SimulationParams m e)
initSimNoEnv updtStrat shuffAs rngSeed = 
  initSimulation updtStrat Nothing Nothing shuffAs rngSeed

newAgentId :: Monad m => SimulationParams m e -> AgentId
newAgentId SimulationParams { simIdGen = idGen } = 
  incrementAtomicallyUnsafe idGen

initRng :: Maybe Int -> IO ()
initRng Nothing       = return ()
initRng (Just seed)   = setStdGen $ mkStdGen seed