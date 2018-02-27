module FRP.Chimera.Simulation.Init where 
{-
  (
    SimulationParams (..)

  , initRng
  , initSimulation
  ) where

import Control.Monad.Random

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Internal

data SimulationParams = SimulationParams 
  { simShuffleAgents :: Bool
  , simRng           :: StdGen
  }

initSimulation :: RandomGen g
               => Bool
               -> Maybe Int
               -> Rand g SimulationParams
initSimulation shuffAs rngSeed = do
  initRng rngSeed

  rng <- getSplit

  return SimulationParams {
    simShuffleAgents = shuffAs
  , simRng = rng
  }

initRng :: Maybe Int -> Rand g ()
initRng Nothing     = return ()
initRng (Just seed) = setStdGen $ mkStdGen seed
-}