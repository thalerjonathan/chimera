{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
module FRP.Chimera.Simulation.ParIteration 
  (
    simulatePar
  ) where

--import Data.Maybe

import Control.Concurrent.STM.TVar
import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.State
import Control.Parallel.Strategies
import qualified Data.Map as Map
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
type FeedbackData m o d = (SimulationParams, [Agent m o d], [AgentIn o d])

simulatePar :: Monad m
            => SimulationParams
            -> [Agent m o d]
            -> [AgentIn o d]
            -> SF m () (SimulationStepOut o)
simulatePar p0 sfs0 ins0 = loopPre (p0, sfs0, ins0) simulateParAux
  where
    simulateParAux :: Monad m 
                   => SF m 
                        ((), FeedbackData m o d)
                        ((SimulationStepOut o), FeedbackData m o d)
    simulateParAux = proc (_, (params, sfs, ins)) -> do
      -- iterate agents in parallel
      (sfs', outs) <- runAgents -< (sfs, ins)

      -- create next inputs and sfs (distribute messages and add/remove new/killed agents)
      let (sfs'', ins') = nextStep ins outs sfs'
      -- create observable outputs
      let obs = observableAgents (map aiId ins) outs

      -- NOTE: ignoring shuffling for now
      {-
      -- TODO: do NOT shuffle => must not make a difference
      -- NOTE: shuffling may seem strange in parallel but this will ensure random message-distribution when required
      (params'', sfsShuffled, insShuffled) = shuffleAgents params' sfs'' ins'
      -}

      t <- time -< ()

      returnA -< ((t, obs), (params, sfs'', ins'))

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

  where
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

nextStep :: [AgentIn o d]
         -> [AgentOut m o d]
         -> [Agent m o d]
         -> ([Agent m o d], [AgentIn o d])
nextStep oldAgentIns newAgentOuts asfs = (asfs', newAgentIns')
  where
    (asfs', newAgentIns) = processAgents asfs oldAgentIns newAgentOuts
    -- NOTE: need to use oldAgentIns as each index corresponds to the agent in newAgentOuts
    newAgentOutsWithAis = map (\(ai, ao) -> (aiId ai, ao)) (zip oldAgentIns newAgentOuts) 
    newAgentIns' = distributeData newAgentIns newAgentOutsWithAis

    processAgents :: [Agent m o d]
                  -> [AgentIn o d]
                  -> [AgentOut m o d]
                  -> ([Agent m o d], [AgentIn o d])
    processAgents asfs oldIs newOs = foldr handleAgent ([], []) asfsIsOs
      where
        asfsIsOs = zip3 asfs oldIs newOs

        handleAgent :: (Agent m o d, AgentIn o d, AgentOut m o d)
                    -> ([Agent m o d], [AgentIn o d])
                    -> ([Agent m o d], [AgentIn o d])
        handleAgent a@(_, oldIn, newOut) acc = handleKillOrLiveAgent acc' a
          where
            idGen = aiIdGen oldIn
            acc' = handleCreateAgents idGen newOut acc 

        handleKillOrLiveAgent :: ([Agent m o d], [AgentIn o d])
                              -> (Agent m o d, AgentIn o d, AgentOut m o d)
                              -> ([Agent m o d], [AgentIn o d])
        handleKillOrLiveAgent acc@(asfsAcc, ainsAcc) (sf, oldIn, newOut)
            | killAgent = acc
            | otherwise = (sf : asfsAcc, newIn : ainsAcc) 
          where
            killAgent = isEvent $ aoKill newOut
            newIn = newAgentIn oldIn

handleCreateAgents :: TVar Int
                   -> AgentOut m o d
                   -> ([Agent m o d], [AgentIn o d])
                   -> ([Agent m o d], [AgentIn o d])
handleCreateAgents idGen ao acc@(asfsAcc, ainsAcc) 
    | not $ null newAgentDefs = (asfsAcc ++ newSfs, ainsAcc ++ newAis)
    | otherwise = acc
  where
    newAgentDefs = aoCreate ao
    newSfs = map adBeh newAgentDefs
    newAis = map (startingAgentInFromAgentDef idGen) newAgentDefs

distributeData :: [AgentIn o d] 
               -> [(AgentId, AgentOut m o d)] 
               -> [AgentIn o d]
distributeData ains aouts = parMap rpar (distributeDataAux allData) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allData = collectAllData aouts

    distributeDataAux :: Map.Map AgentId [AgentData d]
                      -> AgentIn o d
                      -> AgentIn o d
    distributeDataAux allData ain = ain'
      where
        receiverId = aiId ain
        ds = aiData ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverData = Map.lookup receiverId allData
        ds' = maybe ds (\receiverData -> receiverData ++ ds) mayReceiverData

        ain' = ain { aiData = ds' }

collectAllData :: [(AgentId, AgentOut m o d)] -> Map.Map AgentId [AgentData d]
collectAllData aos = foldr collectAllDataAux Map.empty aos
  where
    collectAllDataAux :: (AgentId, AgentOut m o d)
                      -> Map.Map AgentId [AgentData d]
                      -> Map.Map AgentId [AgentData d]
    collectAllDataAux (senderId, ao) accData 
        | not $ null ds = foldr collectAllDataAuxAux accData ds
        | otherwise = accData
      where
        ds = aoData ao

        collectAllDataAuxAux :: AgentData d
                             -> Map.Map AgentId [AgentData d]
                             -> Map.Map AgentId [AgentData d]
        collectAllDataAuxAux (receiverId, m) accData = accData'
          where
            d = (senderId, m)
            mayReceiverData = Map.lookup receiverId accData
            newData = maybe [d] (\receiverData -> d : receiverData) mayReceiverData

            -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
            accData' = seq newData (Map.insert receiverId newData accData)