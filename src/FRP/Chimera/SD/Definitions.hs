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
  --, flowOutToM
  --, flowOutToS

  , stockOutTo
  --, stockOutToM
  --, stockOutToS
) where

import Data.Functor.Identity

import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Common 
import FRP.Chimera.Simulation.Simulation

data SDMsg          = Value Double deriving (Eq, Show)
type SDStockState   = Double
type StockId        = AgentId
type FlowId         = AgentId

type SDDef          = AgentDef Identity SDStockState SDMsg ()
type SD             = Agent Identity SDStockState SDMsg ()
type SDIn           = AgentIn SDStockState SDMsg ()
type SDOut          = AgentOut Identity SDStockState SDMsg ()
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

{-
flowOutToM :: Double -> AgentId -> StateT SDOut Identity ()
flowOutToM = valueOutToM

flowOutToS :: SF (StateT SDOut Identity) (Double, AgentId) () 
flowOutToS = valueOutToS
-}

stockOutTo :: Double -> AgentId -> SDOut -> SDOut
stockOutTo = valueOutTo

{-
stockOutToM :: Double -> AgentId -> StateT SDOut Identity ()
stockOutToM = valueOutToM

stockOutToS :: SF (StateT SDOut Identity) (Double, AgentId) () 
stockOutToS = valueOutToS 
-}

runSD :: [SDDef] -> DTime -> Time -> [(Time, [SDObservable])]
runSD initSdDefs dt t = runIdentity sdObsEnvM
  where
    sdObsEnvM = simulateTime 
                  initSdDefs 
                  dt 
                  t

------------------------------------------------------------------------------------------------------------------------
-- UTILS
------------------------------------------------------------------------------------------------------------------------
filterMessageValue :: DataFlow SDMsg -> Double -> Double
filterMessageValue (_, Value v) _ = v

valueInFrom :: AgentId -> SDIn -> Double
valueInFrom senderId ain = onDataFlowFrom senderId filterMessageValue ain 0.0 

valueOutTo :: Double -> AgentId -> SDOut -> SDOut
valueOutTo value receiverId ao = dataFlow (receiverId, Value value) ao

{-
valueOutToM :: Double -> AgentId -> StateT SDOut Identity ()
valueOutToM value receiverId = dataFlowM (receiverId, Value value) 

valueOutToS :: SF (StateT SDOut Identity) (Double, AgentId) ()  --MonadState (AgentOut Identity o d e) Identity
valueOutToS = proc (value, receiverId) -> do
  dataFlowS -< (receiverId, Value value) 
  -}
------------------------------------------------------------------------------------------------------------------------