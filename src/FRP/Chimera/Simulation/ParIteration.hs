{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
module FRP.Chimera.Simulation.ParIteration 
  (
    simulatePar
  ) where

--import Data.Maybe

--import Control.Concurrent.STM.TVar
import Control.Monad.State
import Control.Monad.Trans.MSF.Reader
--import Control.Parallel.Strategies
--import qualified Data.Map as Map
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Init
--import FRP.Chimera.Simulation.Internal
import FRP.Chimera.Simulation.Common

-- | Steps the simulation using a parallel update-strategy. 
-- Conversations and Recursive Simulation is NOT possible using this strategy.
-- In this strategy each agents SF is run after the same time, actions 
-- are only seen in the next step. This makes
-- this strategy work basically as a map (as opposed to fold in the sequential case).
-- Although the agents make the move at the same time, when shuffling them, 
-- the order of collecting and distributing the messages makes a difference 
-- if model-semantics are relying on randomized message-ordering, 
-- then shuffling is required and has to be turned on in the params.
--
-- An agent which kills itself will still have all its output processed
-- meaning that newly created agents and sent messages are not discharged.
--
-- It is not possible to send messages to currently non-existing agents,
-- also not to agents which may exist in the future. Messages which
-- have as receiver a non-existing agent are discharged without any notice
-- (a minor exception is the sending of messages to newly spawned agents
-- within the iteration when they were created: although they are not running
-- yet, they are known already to the system and will run in the next step).

-- for internal use only
type FeedbackData m o d e = (SimulationParams m e, [Agent m o d e], [AgentIn o d e], e)

simulatePar :: Monad m
            => SimulationParams m e
            -> [Agent m o d e]
            -> [AgentIn o d e]
            -> e
            -> SF m () (SimulationStepOut o e)
simulatePar p0 sfs0 ins0 e0 = loopPre (p0, sfs0, ins0, e0) simulateParAux
  where
    simulateParAux :: Monad m 
                   => SF m 
                        ((), FeedbackData m o d e)
                        ((SimulationStepOut o e), FeedbackData m o d e)
    simulateParAux = proc (_, (params, sfs, ins, e)) -> do
      -- iterate agents in parallel
      (_sfs', outs, es) <- runAgents -< (sfs, ins, e)

      {-
      -- create next inputs and sfs (distribute messages and add/remove new/killed agents)
      (sfs'', ins') = nextStep ins outs sfs'
      -- collapse all environments into one
      (e', params') = foldEnvironments dt params envs e
      -- create observable outputs
      -}
      let obs = observableAgents (map aiId ins) outs

      {-
      -- TODO: do NOT shuffle => must not make a difference
      -- NOTE: shuffling may seem strange in parallel but this will ensure random message-distribution when required
      (params'', sfsShuffled, insShuffled) = shuffleAgents params' sfs'' ins'
      -}

      t <- time -< ()
      let e = head es

      returnA -< ((t, obs, e), (params, sfs, ins, e))

runAgents :: Monad m 
          => SF m 
              ([Agent m o d e], [AgentIn o d e], e) 
              ([Agent m o d e], [AgentOut m o d e], [e])
runAgents = readerS $ proc (dt, (sfs, ins, e)) -> do
    let asIns = zipWith (\sf ain -> (dt, (sf, ain, e))) sfs ins
    as <- mapMSF (runReaderS runAgent) -< asIns
    returnA -< unzip3 as

  where
    runAgent :: Monad m 
             => SF m 
                  (Agent m o d e, AgentIn o d e, e)
                  (Agent m o d e, AgentOut m o d e, e)
    runAgent = arrM runAgentAux
      where
        runAgentAux :: Monad m
                    => (Agent m o d e, AgentIn o d e, e)
                    -> ReaderT Double m (Agent m o d e, AgentOut m o d e, e)
        runAgentAux (sf, ain, e) = do
          stateMon <- unMSF sf (ain, e)
          let ret = runStateT stateMon agentOut
          let ((e', sf'), ao) = ret
          return (sf', ao, e')
      {-
foldEnvironments :: Double 
                 -> SimulationParams e 
                 -> [e] 
                 -> e 
                 -> (e, SimulationParams e)
foldEnvironments dt params allEnvs defaultEnv
    | isFoldingEnv = runEnv dt params foldedEnv 
    | otherwise = (defaultEnv, params)
  where
    isFoldingEnv = isJust mayEnvFoldFun

    mayEnvFoldFun = simEnvFold params
    envFoldFun = fromJust mayEnvFoldFun

    foldedEnv = envFoldFun allEnvs 

nextStep :: [AgentIn o d e]
         -> [AgentOut s m e]
         -> [Agent m o d e]
         -> ([Agent m o d e], [AgentIn o d e])
nextStep oldAgentIns newAgentOuts asfs = (asfs', newAgentIns')
  where
    (asfs', newAgentIns) = processAgents asfs oldAgentIns newAgentOuts
    -- NOTE: need to use oldAgentIns as each index corresponds to the agent in newAgentOuts
    newAgentOutsWithAis = map (\(ai, ao) -> (aiId ai, ao)) (zip oldAgentIns newAgentOuts) 
    newAgentIns' = distributeMessages newAgentIns newAgentOutsWithAis

    processAgents :: [Agent m o d e]
                  -> [AgentIn o d e]
                  -> [AgentOut s m e]
                  -> ([Agent m o d e], [AgentIn o d e])
    processAgents asfs oldIs newOs = foldr handleAgent ([], []) asfsIsOs
      where
        asfsIsOs = zip3 asfs oldIs newOs

        handleAgent :: (Agent m o d e, AgentIn o d e, AgentOut s m e)
                    -> ([Agent m o d e], [AgentIn o d e])
                    -> ([Agent m o d e], [AgentIn o d e])
        handleAgent a@(_, oldIn, newOut) acc = handleKillOrLiveAgent acc' a
          where
            idGen = aiIdGen oldIn
            acc' = handleCreateAgents idGen newOut acc 

        handleKillOrLiveAgent :: ([Agent m o d e], [AgentIn o d e])
                              -> (Agent m o d e, AgentIn o d e, AgentOut s m e)
                              -> ([Agent m o d e], [AgentIn o d e])
        handleKillOrLiveAgent acc@(asfsAcc, ainsAcc) (sf, oldIn, newOut)
            | killAgent = acc
            | otherwise = (sf : asfsAcc, newIn : ainsAcc) 
          where
            killAgent = isEvent $ aoKill newOut
            newIn = newAgentIn oldIn

handleCreateAgents :: TVar Int
                   -> AgentOut s m e
                   -> ([Agent m o d e], [AgentIn o d e])
                   -> ([Agent m o d e], [AgentIn o d e])
handleCreateAgents idGen ao acc@(asfsAcc, ainsAcc) 
    | hasCreateAgents = (asfsAcc ++ newSfs, ainsAcc ++ newAis)
    | otherwise = acc
  where
    newAgentDefsEvt = aoCreate ao
    hasCreateAgents = isEvent newAgentDefsEvt
    newAgentDefs = fromEvent newAgentDefsEvt
    newSfs = map adBeh newAgentDefs
    newAis = map (startingAgentInFromAgentDef idGen) newAgentDefs

distributeMessages :: [AgentIn o d e] -> [(AgentId, AgentOut s m e)] -> [AgentIn o d e]
distributeMessages ains aouts = parMap rpar (distributeMessagesAux allMsgs) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allMsgs = collectAllMessages aouts

    distributeMessagesAux :: Map.Map AgentId [AgentMessage m]
                          -> AgentIn o d e
                          -> AgentIn o d e
    distributeMessagesAux allMsgs ain = ain'
      where
        receiverId = aiId ain
        msgs = aiMessages ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverMsgs = Map.lookup receiverId allMsgs
        msgsEvt = maybe msgs (\receiverMsgs -> mergeMessages (Event receiverMsgs) msgs) mayReceiverMsgs

        ain' = ain { aiMessages = msgsEvt }

collectAllMessages :: [(AgentId, AgentOut s m e)] -> Map.Map AgentId [AgentMessage m]
collectAllMessages aos = foldr collectAllMessagesAux Map.empty aos
  where
    collectAllMessagesAux :: (AgentId, AgentOut s m e)
                          -> Map.Map AgentId [AgentMessage m]
                          -> Map.Map AgentId [AgentMessage m]
    collectAllMessagesAux (senderId, ao) accMsgs 
        | isEvent msgsEvt = foldr collectAllMessagesAuxAux accMsgs (fromEvent msgsEvt)
        | otherwise = accMsgs
      where
        msgsEvt = aoMessages ao

        collectAllMessagesAuxAux :: AgentMessage m
                                 -> Map.Map AgentId [AgentMessage m]
                                 -> Map.Map AgentId [AgentMessage m]
        collectAllMessagesAuxAux (receiverId, m) accMsgs = accMsgs'
          where
            msg = (senderId, m)
            mayReceiverMsgs = Map.lookup receiverId accMsgs
            newMsgs = maybe [msg] (\receiverMsgs -> (msg : receiverMsgs)) mayReceiverMsgs

            -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
            accMsgs' = seq newMsgs (Map.insert receiverId newMsgs accMsgs)

            -}