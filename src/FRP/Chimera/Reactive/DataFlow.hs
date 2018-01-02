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

import Control.Monad.Random
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Environment.Discrete
import FRP.Chimera.Environment.Network
import FRP.Chimera.Random.Stream
import FRP.Chimera.Reactive.Extensions 

type DataSource m o d = SF m (AgentIn o d) (AgentData d)
type DataFlowSF m o d = SF m (AgentIn o d) [AgentData d]

-- TODO: dataFlowRepeatedly
-- TODO: dataFlowAfter
-- TODO: dataFlowOnEvent
-- TODO: dataFlowOnDataReceived

dataFlowOccasionally :: MonadRandom m
                     => Double
                     -> AgentData d
                     -> DataFlowSF m o d
dataFlowOccasionally rate d = dataFlowOccasionallySrc rate (constDataSource d)

dataFlowOccasionallySrc :: MonadRandom m
                        => Double
                        -> DataSource m o d 
                        -> DataFlowSF m o d
dataFlowOccasionallySrc rate dfSrc = proc ain -> do
  sendEvt <- occasionally rate () -< ()
  if isEvent sendEvt 
    then (do
      d <-  dfSrc -< ain
      returnA -< [d])
    else returnA  -< []

dataFlowOccasionallySS :: MonadRandom m
                       => Double
                       -> Int
                       -> AgentData d
                       -> DataFlowSF m o d
dataFlowOccasionallySS rate ss d = dataFlowOccasionallySrcSS rate ss (constDataSource d)

dataFlowOccasionallySrcSS :: MonadRandom m
                          => Double
                          -> Int
                          -> DataSource m o d 
                          -> DataFlowSF m o d
dataFlowOccasionallySrcSS rate ss dfSrc = proc ain -> do
    sendEvtsSS <- superSamplingUniform ss (occasionally rate ())    -< ()
    dfSS       <- superSamplingUniform ss dfSrc                     -< ain
    returnA -< foldr dataFlowOccasionallySrcSSAux [] (zip sendEvtsSS dfSS)
  where
    dataFlowOccasionallySrcSSAux :: (Event (), AgentData d)
                                 -> [AgentData d]
                                 -> [AgentData d]
    dataFlowOccasionallySrcSSAux (NoEvent,  _) acc = acc
    dataFlowOccasionallySrcSSAux (Event (), d) acc = d : acc
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
  randNode <- arrM (\aid -> do
    rn <- lift $ randomNeighbourNode aid e
    return rn) -< aid
  returnA -< (randNode, d)

-- NOTE: assumes state isJust
randomNeighbourCellMsgSource :: MonadRandom m
                             => Discrete2d AgentId
                             -> Discrete2dCoord
                             -> d 
                             -> Bool 
                             -> DataSource m o d 
randomNeighbourCellMsgSource e pos d ic = proc _ -> do
  -- pos <- arrM_ (posFunc <$> lift agentObservableM) -< ()
  randCell <- arrM (\(pos, e) -> do
    c <- lift $ randomNeighbourCell pos ic e
    return c) -< (pos, e)
  returnA -< (randCell, d)

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