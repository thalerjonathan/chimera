{-# LANGUAGE Arrows #-}
module FRP.Chimera.Simulation.Transaction 
  (
    runTransactions
  ) where

import Data.Maybe
import qualified Data.Map as Map
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Init 
import FRP.Chimera.Simulation.Running

-- TODO: refactor code, looks VERY ugly atm

-- TX are executed sequentially
runTransactions :: Monad m
                => IdGen
                -> SF m
                      ([(AgentId, AgentOut m o d)], [Agent m o d])
                      ([(AgentId, AgentOut m o d)], [Agent m o d])
runTransactions idGen = proc (aios, sfs) -> do
    let els = zip aios sfs
    let m = foldr (\((aid, ao), sf) m' -> Map.insert aid (ao, sf) m') Map.empty els
    m' <- runTransactionsAux -< (els, m)
    let ml = Map.toList m'
    let aiosMsfs@(_, _) = foldr (\(aid, (ao, sf)) (accAio, accSf) -> ((aid, ao) : accAio, sf : accSf)) ([], []) ml
    
    -- when there are still TX requests then run them recursively
    let hasTx = any (\(_, (ao, _)) -> isEvent $ aoRequestTx ao) ml
    if hasTx 
      then runTransactions idGen -< aiosMsfs
      else returnA -< aiosMsfs

  where
    runTransactionsAux :: Monad m
                        => SF m
                            (([((AgentId, AgentOut m o d), Agent m o d)]),
                              (Map.Map AgentId (AgentOut m o d, Agent m o d)))
                            (Map.Map AgentId (AgentOut m o d, Agent m o d))
    runTransactionsAux = proc (els, m) -> do
      if null els
        then returnA -< m
        else (do 
          let e@((_, ao), _) = head els
          if (isEvent $ aoRequestTx ao)
            then (do
              m' <- runTxPair -< (e, m)
              runTransactionsAux -< (tail els, m'))
            else runTransactionsAux -< (tail els, m))

    -- care must be taken if two agents want to start a TX with each other at the same time
    -- note that we allow agents to transact with themselves
    runTxPair :: Monad m
              => SF m
                  (((AgentId, AgentOut m o d), Agent m o d),
                    (Map.Map AgentId (AgentOut m o d, Agent m o d)))
                  (Map.Map AgentId (AgentOut m o d, Agent m o d))
    runTxPair = proc (((sAid, sAo0), sSf0), m) -> do
      let ((rAid, d), sTxSf) = fromEvent $ aoRequestTx sAo0
      let mayReceiver = Map.lookup rAid m

      if isNothing mayReceiver
        -- target of the TX request not found, ignoring TX
        then (do
          -- transaction receiver not found, ignoring TX request
          -- set tx-request in agentout to nothing
          let m' = Map.insert sAid (sAo0 { aoRequestTx = NoEvent }, sSf0) m
          returnA -< m')
        else (do
          let (_, rSf0) = fromJust mayReceiver
          
          let rAin = (agentIn rAid idGen) { aiRequestTx = Event (sAid, d) }

          -- ignoring the sf of this run makes it as it has never happened,
          (rAo', _) <- runAgentWithDt 0 -< (rSf0, rAin) 

          let acceptTxEvt = aoAcceptTx rAo'
          if isEvent acceptTxEvt
            -- the request has been turned down, no TX
            then (do
              -- transaction request turned down / ignored by receiver
              -- set tx-request in agentout to nothing
              let m' = Map.insert sAid (sAo0 { aoRequestTx = NoEvent }, sSf0) m
              returnA -< m')
            else (do
              let (d, rTxSf) = fromEvent acceptTxEvt

              let rTxAo0 = AgentTXOut {
                aoTxData      = Just d
              , aoTxCommit    = Nothing
              , aoTxAbort     = False
              }

              mayTx <- runTx -< ((sTxSf, sSf0), (rTxAo0, rTxSf, rSf0))
              if isNothing mayTx
                then (do
                  -- transaction aborted
                  -- set tx-request in agentout to nothing
                  let m' = Map.insert sAid (sAo0 { aoRequestTx = NoEvent }, sSf0) m
                  returnA -< m')
                else (do
                  -- transaction finished, committing
                  let ((sAo', sSf'), (rAo', rSf')) = fromJust mayTx

                  --printDebugS -< ("aid1 = " ++ show aid1 ++ ", ao1 = " ++ show ao1)
                  --printDebugS -< ("aid2 = " ++ show aid2 ++ ", ao2 = " ++ show ao2)
                  
                  let m' = Map.insert sAid (sAo', sSf') m
                  let m'' = Map.insert rAid (rAo', rSf') m'
                  
                  -- transaction committed...
                  returnA -< m'')))

runTx :: Monad m
      => SF m
            ((AgentTX m o d, Agent m o d),
            (AgentTXOut m o d, AgentTX m o d, Agent m o d))
          (Maybe
            ((AgentOut m o d, Agent m o d), 
              (AgentOut m o d, Agent m o d)))
runTx = proc ((sTxSf0, sSf), (rTxAo0, rTxSf0, rSf)) -> do
  let sTxAin = AgentTXIn {
      aiTxData      = aoTxData rTxAo0
    , aiTxCommit    = isJust $ aoTxCommit rTxAo0
    , aiTxAbort     = aoTxAbort rTxAo0
    }

  (sTxAo, sTxSf') <- runAgentTx -< (sTxSf0, sTxAin)
  
  let rTxAin = AgentTXIn {
    aiTxData      = aoTxData sTxAo
  , aiTxCommit    = isJust $ aoTxCommit sTxAo
  , aiTxAbort     = aoTxAbort sTxAo
  }

  (rTxAo', rTxSf') <- runAgentTx -< (rTxSf0, rTxAin)
  
  -- either one aborts the TX, abort the whole TX
  if aoTxAbort sTxAo || aoTxAbort rTxAo'
    then returnA -< Nothing
    else (do
      -- if both commit, we commit the TX as a whole
      -- otherwise we continue with another TX step
      if (isJust $ aoTxCommit sTxAo) && (isJust $ aoTxCommit rTxAo')
        then (do
          let (sAo, maySsf) = fromJust $ aoTxCommit sTxAo
          let (rAo, mayRsf) = fromJust $ aoTxCommit rTxAo'
          let sSf' = fromMaybe sSf maySsf
          let rSf' = fromMaybe rSf mayRsf
          
          returnA -< Just ((sAo, sSf'), (rAo, rSf')))
        -- if not both commit we assume that another TX step is required.
        else runTx -< ((sTxSf', sSf), (rTxAo', rTxSf', rSf)))