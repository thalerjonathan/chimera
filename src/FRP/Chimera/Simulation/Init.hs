module FRP.Chimera.Simulation.Init 
  (
    SimulationParams (..)

  , initRng
  , initSimulation
  , newAgentId
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad.Random

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Internal

data SimulationParams = SimulationParams 
  {
    simShuffleAgents  :: Bool
  , simRng            :: StdGen
  , simIdGen          :: TVar Int
  }

initSimulation :: Bool
               -> Maybe Int
               -> IO SimulationParams
initSimulation shuffAs rngSeed = do
  initRng rngSeed

  rng <- getSplit
  agentIdVar <- newTVarIO 0

  return SimulationParams {
      simShuffleAgents = shuffAs
    , simRng = rng
    , simIdGen = agentIdVar
    }

newAgentId :: SimulationParams -> AgentId
newAgentId SimulationParams { simIdGen = idGen } = 
  incrementAtomicallyUnsafe idGen

initRng :: Maybe Int -> IO ()
initRng Nothing       = return ()
initRng (Just seed)   = setStdGen $ mkStdGen seed