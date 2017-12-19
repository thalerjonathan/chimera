{-# LANGUAGE Arrows #-}
module FRP.Chimera.Simulation.Running 
  (
    runAgents
  , runAgent
  , runAgentWithDt
  , runAgentTx
  ) where

import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.State
import FRP.BearRiver

import FRP.Chimera.Agent.Interface

-- deep magic going on here... thx to Manuel Bärenz and Ivan Perez
-- NOTE: we can also run agents with a dt of 0 here if required
runAgents :: Monad m 
          => SF m 
              ([Agent m o d], [AgentIn o d]) 
              ([Agent m o d], [AgentOut m o d])
runAgents = readerS $ proc (dt, (sfs, ins)) -> do
    let asIns        = zipWith (\sf ain -> (dt, (sf, ain))) sfs ins
    
    arets <- mapMSF (runReaderS runAgent) -< asIns
    let (aos, sfs') = unzip arets
    returnA -< (sfs', aos)

runAgent :: Monad m 
          => SF m 
              (Agent m o d, AgentIn o d)
              (AgentOut m o d, Agent m o d)
runAgent = runStateSF_ runAgentAux agentOut
  where
    runAgentAux :: Monad m
                => SF (StateT (AgentOut m o d) m) 
                    (Agent m o d, AgentIn o d) 
                    (Agent m o d)
    runAgentAux = arrM (\(sf, ain) -> do
      -- returning (), as we are only interested in the effects on agentout
      (_, sf') <- unMSF sf ain
      return sf')

    runStateSF_ :: Monad m => SF (StateT s m) a b -> s -> SF m a (s, b)
    runStateSF_ sf = runStateS_ $ liftMSFPurer commute sf

    -- deep magic going on as well...
    commute :: Monad m => ReaderT r (StateT s m) a -> StateT s (ReaderT r m) a
    commute rt = 
      StateT (\s -> 
        ReaderT (\r -> let st = runReaderT rt r
                        in runStateT st s))

runAgentWithDt :: Monad m
               => Double
               -> SF m
                    (Agent m o d, AgentIn o d)
                    (AgentOut m o d, Agent m o d)
runAgentWithDt dt = readerS $ proc (_, (sf, ain)) -> do
  (ao, sf') <- runReaderS_ runAgent dt -< (sf, ain)
  returnA -< (ao, sf')

-- NOTE: TXs always run with dt = 0
runAgentTx :: Monad m
           => SF m
                (AgentTX m o d, AgentTXIn d)
                (AgentTXOut m o d, AgentTX m o d)
runAgentTx = readerS $ proc (_, (txSf, txIn)) -> do
  (txOut, txSf') <- runReaderS_ (arrM (\(txSf, txIn) -> unMSF txSf txIn)) 0 -< (txSf, txIn)
  returnA -< (txOut, txSf')