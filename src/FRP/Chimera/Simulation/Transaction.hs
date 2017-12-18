{-# LANGUAGE Arrows #-}
module FRP.Chimera.Simulation.Common 
  (
    runTransactions
  ) where

    -- TX need to be executed sequentially...
    runTransactions :: RandomGen g
                    => SF (TXTestMonadStack g)
                          ([(AgentId, AgentOut (TXTestMonadStack g) o d)], [Agent (TXTestMonadStack g) o d])
                          ([(AgentId, AgentOut (TXTestMonadStack g) o d)], [Agent (TXTestMonadStack g) o d])
    runTransactions = proc (aios, sfs) -> do
        let els = zip aios sfs
        -- printDebugS -< ("els = " ++ show aios)
        let m = foldr (\((aid, ao), sf) m' -> Map.insert aid (ao, sf) m') Map.empty els
        m' <- runTransactionsAux -< (els, m)
        let ml = Map.toList m'
        let aiosMsfs@(_, _) = foldr (\(aid, (ao, sf)) (accAio, accSf) -> ((aid, ao) : accAio, sf : accSf)) ([], []) ml
        
        -- when there are still TX requests then run them recursively
        let hasTx = any (\(_, (ao, _)) -> isJust $ aoRequestTx ao) ml
        if hasTx 
          then runTransactions -< aiosMsfs
          else returnA -< aiosMsfs

      where
        runTransactionsAux :: RandomGen g
                           => SF (TXTestMonadStack g)
                                (([((AgentId, AgentOut (TXTestMonadStack g) o d), Agent (TXTestMonadStack g) o d)]),
                                  (Map.Map AgentId (AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d)))
                                (Map.Map AgentId (AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d))
        runTransactionsAux = proc (els, m) -> do
          if null els
            then returnA -< m
            else (do 
              let e@((_, ao), _) = head els
              if (isJust $ aoRequestTx ao)
                then (do
                  printDebugS -< ("found TX pair, running TX...")
                  m' <- runTxPair -< (e, m)
                  runTransactionsAux -< (tail els, m'))
                else runTransactionsAux -< (tail els, m))

        -- care must be taken if two agents want to start a TX with each other at the same time
        -- note that we allow agents to transact with themselves
        runTxPair :: RandomGen g
                  => SF (TXTestMonadStack g)
                      (((AgentId, AgentOut (TXTestMonadStack g) o d), Agent (TXTestMonadStack g) o d),
                        (Map.Map AgentId (AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d)))
                      (Map.Map AgentId (AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d))
        runTxPair = proc (((sAid, sAo0), sSf0), m) -> do
          let ((rAid, d), sTxSf) = fromJust $ aoRequestTx sAo0
          let mayReceiver = Map.lookup rAid m

          if isNothing mayReceiver
            -- target of the TX request not found, ignoring TX
            then (do
              -- set tx-request in agentout to nothing
              let m' = Map.insert sAid (sAo0 { aoRequestTx = Nothing }, sSf0) m
              printDebugS -< ("transaction receiver not found, ignoring TX request")
              returnA -< m')
            else (do
              let (_, rSf0) = fromJust mayReceiver
              
              let rAin = (agentIn rAid) { aiRequestTx = Just (sAid, d) }

              -- ignoring the sf of this run makes it as it has never happened,
              (rAo', _) <- runAgentWithDt 0 -< (rAin, rSf0) 

              let mayAcceptTx = aoAcceptTx rAo'
              if isNothing mayAcceptTx
                -- the request has been turned down, no TX
                then (do
                  -- set tx-request in agentout to nothing
                  let m' = Map.insert sAid (sAo0 { aoRequestTx = Nothing }, sSf0) m
                  printDebugS -< ("transaction request turned down / ignored by receiver")
                  returnA -< m')
                else (do
                  let (d, rTxSf) = fromJust mayAcceptTx

                  let rTxAo0 = AgentTXOut {
                    aoTxData      = Just d
                  , aoTxCommit    = Nothing
                  , aoTxAbort     = False
                  }

                  mayTx <- runTx -< ((sTxSf, sSf0), (rTxAo0, rTxSf, rSf0))
                  if isNothing mayTx
                    then (do
                      -- set tx-request in agentout to nothing
                      let m' = Map.insert sAid (sAo0 { aoRequestTx = Nothing }, sSf0) m
                      printDebugS -< ("transaction aborted")
                      returnA -< m')
                    else (do
                      printDebugS -< ("transaction finished, committing...")
                      let ((sAo', sSf'), (rAo', rSf')) = fromJust mayTx

                      --printDebugS -< ("aid1 = " ++ show aid1 ++ ", ao1 = " ++ show ao1)
                      --printDebugS -< ("aid2 = " ++ show aid2 ++ ", ao2 = " ++ show ao2)
                      
                      let m' = Map.insert sAid (sAo', sSf') m
                      let m'' = Map.insert rAid (rAo', rSf') m'
                      
                      printDebugS -< ("transaction committed")
                      returnA -< m'')))

    runTx :: RandomGen g
          => SF (TXTestMonadStack g)
               ((AgentTX (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d),
                (AgentTXOut (TXTestMonadStack g) o d, AgentTX (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d))
              (Maybe
                ((AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d), 
                 (AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d)))
    runTx = proc ((sTxSf0, sSf), (rTxAo0, rTxSf0, rSf)) -> do
      let sTxAin = AgentTXIn {
          aiTxData      = aoTxData rTxAo0
        , aiTxCommit    = isJust $ aoTxCommit rTxAo0
        , aiTxAbort     = aoTxAbort rTxAo0
        }

      (sTxAo, sTxSf') <- runAgentTx -< (sTxAin, sTxSf0)
      
      let rTxAin = AgentTXIn {
        aiTxData      = aoTxData sTxAo
      , aiTxCommit    = isJust $ aoTxCommit sTxAo
      , aiTxAbort     = aoTxAbort sTxAo
      }

      (rTxAo', rTxSf') <- runAgentTx -< (rTxAin, rTxSf0)
      
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

runAgents :: RandomGen g
          => SF (TXTestMonadStack g)
              ([Agent (TXTestMonadStack g) o d], [AgentIn d]) 
              ([Agent (TXTestMonadStack g) o d], [AgentOut (TXTestMonadStack g) o d])
runAgents = readerS $ proc (dt, (sfs, ins)) -> do
    let asIns = zipWith (\sf ain -> (dt, (ain, sf))) sfs ins
    arets <- mapMSF (runReaderS runAgent) -< asIns
    let (aos, sfs') = unzip arets
    returnA -< (sfs', aos)

runAgentWithDt :: RandomGen g
               => Double
               -> SF (TXTestMonadStack g)
                    (AgentIn d, Agent (TXTestMonadStack g) o d)
                    (AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d)
runAgentWithDt dt = readerS $ proc (_, (ain, sf)) -> do
  (ao, sf') <- runReaderS_ runAgent dt -< (ain, sf)
  returnA -< (ao, sf')

-- NOTE: TXs always run with dt = 0
runAgentTx :: RandomGen g
           => SF (TXTestMonadStack g)
                (AgentTXIn d, AgentTX (TXTestMonadStack g) o d)
                (AgentTXOut (TXTestMonadStack g) o d, AgentTX (TXTestMonadStack g) o d)
runAgentTx = readerS $ proc (_, (txIn, txSf)) -> do
  (txOut, txSf') <- runReaderS_ (arrM (\(txIn, txSf) -> unMSF txSf txIn)) 0 -< (txIn, txSf)
  returnA -< (txOut, txSf')

runAgent :: RandomGen g
          => SF (TXTestMonadStack g)
              (AgentIn d, Agent (TXTestMonadStack g) o d)
              (AgentOut (TXTestMonadStack g) o d, Agent (TXTestMonadStack g) o d)
runAgent = arrM (\(ain, sf) -> unMSF sf ain)
