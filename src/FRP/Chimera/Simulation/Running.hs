{-# LANGUAGE Arrows #-}
module FRP.Chimera.Simulation.Running 
  (
    runAgents
  , runAgent
  , runAgentWithDt
  , runAgentTx
  ) where

import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver

import FRP.Chimera.Agent.Interface

-- deep magic going on here... thx to Manuel Bärenz and Ivan Perez
-- NOTE: we can also run agents with a dt of 0 here if required
runAgents :: Monad m 
          => SF (ABSMonad m e) 
              ([AgentCont m o d e], [AgentIn o d e]) 
              ([AgentCont m o d e], [AgentOut m o d e])
runAgents = readerS $ proc (dt, (sfs, ins)) -> do
    let asIns = zipWith (\sf ain -> (dt, (sf, ain))) sfs ins
    
    arets <- mapMSF (runReaderS runAgent) -< asIns
    let (aos, sfs') = unzip arets
    returnA -< (sfs', aos)

runAgent :: Monad m 
          => SF (ABSMonad m e) 
              (AgentCont m o d e, AgentIn o d e)
              (AgentOut m o d e, AgentCont m o d e)
runAgent = arrM (\(sf, ain) -> do
  (ao, sf') <- unMSF sf ain
  return (ao, sf'))

runAgentWithDt :: Monad m
               => Double
               -> SF (ABSMonad m e)
                    (AgentCont m o d e, AgentIn o d e)
                    (AgentOut m o d e, AgentCont m o d e)
runAgentWithDt dt = readerS $ proc (_, (sf, ain)) -> do
  (ao, sf') <- runReaderS_ runAgent dt -< (sf, ain)
  returnA -< (ao, sf')

-- NOTE: TXs always run with dt = 0
runAgentTx :: Monad m
           => SF (ABSMonad m e)
                (AgentTX m o d e, AgentTXIn d)
                (AgentTXOut m o d e, AgentTX m o d e)
runAgentTx = readerS $ proc (_, (txSf, txIn)) -> do
  (txOut, txSf') <- runReaderS_ (arrM (uncurry unMSF)) 0 -< (txSf, txIn)
  returnA -< (txOut, txSf')

{-
runAgent :: Monad m 
          => SF m 
              (AgentCont m o d e, AgentIn o d e)
              (AgentOut m o d e, AgentCont m o d e)
runAgent = runStateSF_ runAgentAux agentOut
  where
    runAgentAux :: Monad m
                => SF (StateT (AgentOut m o d e) m) 
                    (AgentCont m o d e, AgentIn o d e) 
                    (AgentCont m o d e)
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
                    (AgentCont m o d e, AgentIn o d e)
                    (AgentOut m o d e, AgentCont m o d e)
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
  -}