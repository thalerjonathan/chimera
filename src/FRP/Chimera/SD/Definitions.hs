{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.SD.Definitions 
  (
    StockId
  , FlowId

  , Stock
  , Flow
  , SDObservable
  , SDDef

  , runSD

  , createStock
  , createFlow

  , flowInFrom
  , stockInFrom

  , flowOutTo
  , flowOutToM
  , flowOutToS

  , stockOutTo
  , stockOutToM
  , stockOutToS
) where

import Data.Functor.Identity
import System.Random (StdGen, mkStdGen)

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.State.Strict
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Monad
import FRP.Chimera.Agent.Stream
import FRP.Chimera.Simulation.Common 
import FRP.Chimera.Simulation.Simulation
import FRP.Chimera.Simulation.Init

data SDMsg          = Value Double deriving (Eq, Show)
type SDStockState   = Double
type StockId        = AgentId
type FlowId         = AgentId

type SDDef          = AgentDef Identity SDStockState SDMsg 
type SD             = Agent Identity SDStockState SDMsg 
type SDIn           = AgentIn SDStockState SDMsg 
type SDOut          = AgentOut Identity SDStockState SDMsg 
type SDObservable   = AgentObservable SDStockState

type Stock          = Double -> SD
type Flow           = SD

createStock :: AgentId 
            -> SDStockState
            -> Stock
            -> SDDef
createStock stockId stockState stockBeh = AgentDef { 
    adId        = stockId
  , adBeh       = stockBeh stockState
  , adInitData  = []
  }

createFlow :: AgentId 
           -> Flow
           -> SDDef
createFlow flowId flowBeh = AgentDef { 
    adId        = flowId
  , adBeh       = flowBeh
  , adInitData  = []
  }

flowInFrom :: AgentId -> SDIn -> Double
flowInFrom = valueInFrom

stockInFrom :: AgentId -> SDIn -> Double
stockInFrom = valueInFrom

flowOutTo :: Double -> AgentId -> SDOut -> SDOut
flowOutTo = valueOutTo

flowOutToM :: Double -> AgentId -> StateT SDOut Identity ()
flowOutToM = valueOutToM

flowOutToS :: SF (StateT SDOut Identity) (Double, AgentId) () 
flowOutToS = valueOutToS

stockOutTo :: Double -> AgentId -> SDOut -> SDOut
stockOutTo = valueOutTo

stockOutToM :: Double -> AgentId -> StateT SDOut Identity ()
stockOutToM = valueOutToM

stockOutToS :: SF (StateT SDOut Identity) (Double, AgentId) () 
stockOutToS = valueOutToS 

runSD :: [SDDef] -> DTime -> Time -> [(Time, [SDObservable])]
runSD initSdDefs dt t = runIdentity sdObsEnvM
  where
    sdObsEnvM = simulateTime 
                  initSdDefs 
                  params 
                  dt 
                  t

    -- SystemDynamics MUST NOT rely on RNGs at all, so no need to initialize it
    -- SystemDynamics MUST ABSOLUTELY only run Parllel and there is no need to shuffle the agents (=stocks)
    params = SimulationParams {
      simShuffleAgents  = False
    , simRng            = dummyRng
    , simIdGen          = dummyIdGen
    }

    dummyIdGen = unsafePerformIO  $ newTVarIO 0

------------------------------------------------------------------------------------------------------------------------
-- UTILS
------------------------------------------------------------------------------------------------------------------------
-- NOTE: SD is completely deterministic but we need to provide some RNG for the AgentDef
dummyRng :: StdGen
dummyRng = mkStdGen 0

filterMessageValue :: (AgentData SDMsg) -> Double -> Double
filterMessageValue (_, Value v) _ = v

valueInFrom :: AgentId -> SDIn -> Double
valueInFrom senderId ain = onDataFlowFrom senderId filterMessageValue ain 0.0 

valueOutTo :: Double -> AgentId -> SDOut -> SDOut
valueOutTo value receiverId ao = dataFlow (receiverId, Value value) ao

valueOutToM :: Double -> AgentId -> StateT SDOut Identity ()
valueOutToM value receiverId = dataFlowM (receiverId, Value value) 

valueOutToS :: SF (StateT SDOut Identity) (Double, AgentId) ()  --MonadState (AgentOut Identity o d e) Identity
valueOutToS = proc (value, receiverId) -> do
  dataFlowS -< (receiverId, Value value) 
------------------------------------------------------------------------------------------------------------------------