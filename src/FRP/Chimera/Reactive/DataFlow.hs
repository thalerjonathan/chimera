{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Reactive.DataFlow
  (
    DataSource
  , DataFlowSF

  , dataFlowOccasionally
  , dataFlowOccasionallySrc
  
  , dataFlowOccasionallySS
  , dataFlowOccasionallySrcSS

  , constDataReceiverSource
  , constDataSource
  , randomNeighbourNodeMsgSource
  , randomNeighbourCellMsgSource
  , randomAgentIdMsgSource
  ) where

import Control.Monad.State.Strict
import Control.Monad.Random
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Monad
import FRP.Chimera.Agent.Stream
import FRP.Chimera.Environment.Discrete
import FRP.Chimera.Environment.Network
import FRP.Chimera.Random.Stream
import FRP.Chimera.Reactive.Extensions 

-- TODO: is StateT really necessary or can we go with a generic Monad m only? 
type DataSource m o d = SF (StateT (AgentOut m o d) m) (AgentIn o d) (AgentData d)
-- NOTE: StateT is necessary here because we are calling dataFlowS which requires
-- StateT
type DataFlowSF m o d = SF (StateT (AgentOut m o d) m) (AgentIn o d) ()

-- TODO: dataFlowRepeatedly
-- TODO: dataFlowAfter
-- TODO: dataFlowOnEvent
-- TODO: dataFlowOnDataReceived

dataFlowOccasionally :: (MonadRandom m, MonadState (AgentOut m o d) m)
                     => Double
                     -> AgentData d
                     -> DataFlowSF m o d
dataFlowOccasionally rate d = dataFlowOccasionallySrc rate (constDataSource d)

dataFlowOccasionallySrc :: (MonadRandom m, MonadState (AgentOut m o d) m)
                        => Double
                        -> DataSource m o d 
                        -> DataFlowSF m o d
dataFlowOccasionallySrc rate dfSrc = proc ain -> do
  sendEvt <- occasionally rate () -< ()
  if isEvent sendEvt 
    then (do
      -- TODO: can we formulate this in point-free style?
      d <-  dfSrc -< ain
      dataFlowS   -< d)
    else returnA  -< ()

dataFlowOccasionallySS :: (MonadRandom m, MonadState (AgentOut m o d) m)
                       => Double
                       -> Int
                       -> AgentData d
                       -> DataFlowSF m o d
dataFlowOccasionallySS rate ss d = dataFlowOccasionallySrcSS rate ss (constDataSource d)

dataFlowOccasionallySrcSS :: (MonadRandom m, MonadState (AgentOut m o d) m)
                          => Double
                          -> Int
                          -> DataSource m o d 
                          -> DataFlowSF m o d
dataFlowOccasionallySrcSS rate ss dfSrc = proc ain -> do
    sendEvtsSS <- superSamplingUniform ss (occasionally rate ())    -< ()
    dfSS       <- superSamplingUniform ss dfSrc                     -< ain
    _          <- arrM $ mapM (lift . dataFlowOccasionallySrcSSAux) -< (zip sendEvtsSS dfSS)
    returnA -< ()
  where
    dataFlowOccasionallySrcSSAux :: MonadState (AgentOut m o d) m
                                 => (Event (), AgentData d)
                                 -> (StateT (AgentOut m o d) m) ()
    dataFlowOccasionallySrcSSAux (NoEvent,  _) = return ()
    dataFlowOccasionallySrcSSAux (Event (), d) = dataFlowM d
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constDataReceiverSource :: Monad m => d -> AgentId -> DataSource m o d
constDataReceiverSource d receiver = constant (receiver, d)

constDataSource :: Monad m => AgentData d -> DataSource m o d
constDataSource = constant

randomNeighbourNodeMsgSource :: MonadRandom m
                             => Network l
                             -> d
                             -> DataSource m o d
randomNeighbourNodeMsgSource e d = proc ain -> do
  let aid = agentId ain
  -- TODO: can we formulate this in point-free style?
  randNode <- arrM (\aid -> do
    rn <- lift $ randomNeighbourNode aid e
    return rn) -< aid
  returnA -< (randNode, d)

-- NOTE: assumes state isJust
randomNeighbourCellMsgSource :: MonadRandom m
                             => Discrete2d AgentId
                             -> (o -> Discrete2dCoord) 
                             -> d 
                             -> Bool 
                             -> DataSource m o d 
randomNeighbourCellMsgSource e posFunc d ic = proc _ -> do
  pos <- arrM_ (posFunc <$> lift agentObservableM) -< ()
  -- TODO: can we formulate this in point-free style?
  randCell <- arrM (\(pos, e) -> do
    c <- lift $ randomNeighbourCell pos ic e
    return c) -< (pos, e)
  returnA -< (randCell, d)

-- TODO: need a StateT of the Discrete environment
randomAgentIdMsgSource :: MonadRandom m
                       => [AgentId]
                       -> d
                       -> Bool 
                       -> DataSource m o d 
randomAgentIdMsgSource agentIds d ignoreSelf = proc ain -> do
  let aid = agentId ain
  randAid <- randomElemS_ -< agentIds
  if ignoreSelf && aid == randAid
    then randomAgentIdMsgSource agentIds d ignoreSelf -< ain
    else returnA -< (randAid, d)
-------------------------------------------------------------------------------