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

type DataSource m o d e = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) (AgentData d)
type DataFlowSF m o d e = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) ()

-- TODO: dataFlowRepeatedly
-- TODO: dataFlowAfter
-- TODO: dataFlowOnEvent
-- TODO: dataFlowOnDataReceived

dataFlowOccasionally :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                     => Double
                     -> AgentData d
                     -> DataFlowSF m o d e
dataFlowOccasionally rate d = dataFlowOccasionallySrc rate (constDataSource d)

dataFlowOccasionallySrc :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                        => Double
                        -> DataSource m o d e 
                        -> DataFlowSF m o d e
dataFlowOccasionallySrc rate dfSrc = proc (ain, e) -> do
  sendEvt <- occasionally rate () -< ()
  if isEvent sendEvt 
    then (do
      -- TODO: can we formulate this in point-free style?
      d <-  dfSrc -< (ain, e)
      dataFlowS   -< d)
    else returnA  -< ()

dataFlowOccasionallySS :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                       => Double
                       -> Int
                       -> AgentData d
                       -> DataFlowSF m o d e
dataFlowOccasionallySS rate ss d = dataFlowOccasionallySrcSS rate ss (constDataSource d)

dataFlowOccasionallySrcSS :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                          => Double
                          -> Int
                          -> DataSource m o d e 
                          -> DataFlowSF m o d e
dataFlowOccasionallySrcSS rate ss dfSrc = proc aie -> do
    sendEvtsSS <- superSamplingUniform ss (occasionally rate ())    -< ()
    dfSS       <- superSamplingUniform ss dfSrc                     -< aie
    _          <- arrM $ mapM (lift . dataFlowOccasionallySrcSSAux) -< (zip sendEvtsSS dfSS)
    returnA -< ()
  where
    dataFlowOccasionallySrcSSAux :: MonadState (AgentOut m o d e) m
                                 => (Event (), AgentData d)
                                 -> (StateT (AgentOut m o d e) m) ()
    dataFlowOccasionallySrcSSAux (NoEvent,  _) = return ()
    dataFlowOccasionallySrcSSAux (Event (), d) = dataFlowM d
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constDataReceiverSource :: Monad m => d -> AgentId -> DataSource m o d e
constDataReceiverSource d receiver = constant (receiver, d)

constDataSource :: Monad m => AgentData d -> DataSource m o d e
constDataSource = constant

randomNeighbourNodeMsgSource :: MonadRandom m
                             => d
                             -> DataSource m o d (Network l)
randomNeighbourNodeMsgSource d = proc (ain, e) -> do
  let aid = agentId ain
  -- TODO: can we formulate this in point-free style?
  randNode <- arrM (\(aid, e) -> do
    rn <- lift $ randomNeighbourNode aid e
    return rn) -< (aid, e)
  returnA -< (randNode, d)

-- NOTE: assumes state isJust
randomNeighbourCellMsgSource :: MonadRandom m
                             => (o -> Discrete2dCoord) 
                             -> d 
                             -> Bool 
                             -> DataSource m o d (Discrete2d AgentId)
randomNeighbourCellMsgSource posFunc d ic = proc (_, e) -> do
  pos <- arrM_ (posFunc <$> lift agentObservableM) -< ()
  -- TODO: can we formulate this in point-free style?
  randCell <- arrM (\(pos, e) -> do
    c <- lift $ randomNeighbourCell pos ic e
    return c) -< (pos, e)
  returnA -< (randCell, d)

randomAgentIdMsgSource :: MonadRandom m
                       => d
                       -> Bool 
                       -> DataSource m o d [AgentId]
randomAgentIdMsgSource d ignoreSelf = proc aie@(ain, agentIds) -> do
  let aid = agentId ain
  randAid <- randomElemS_ -< agentIds
  if ignoreSelf && aid == randAid
    then randomAgentIdMsgSource d ignoreSelf -< aie
    else returnA -< (randAid, d)
-------------------------------------------------------------------------------