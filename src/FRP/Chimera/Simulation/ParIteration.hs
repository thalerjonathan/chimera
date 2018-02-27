{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
module FRP.Chimera.Simulation.ParIteration 
  (
    simulatePar
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Parallel.Strategies
import qualified Data.Map as Map
import           FRP.BearRiver

import           FRP.Chimera.Agent.Interface
import           FRP.Chimera.Agent.Monad
import           FRP.Chimera.Simulation.Common
import           FRP.Chimera.Simulation.Running
--import FRP.Chimera.Simulation.Transaction

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
type FeedbackData m o d e = ([AgentCont m o d e], [AgentIn o d e])

simulatePar :: Monad m
            => [AgentCont m o d e]
            -> [AgentIn o d e]
            -> SF (ABSMonad m e) () (SimulationStepOut o)
simulatePar sfs0 ins0 = loopPre (sfs0, ins0) simulateParAux
  where
    simulateParAux :: Monad m 
                   => SF (ABSMonad m e)
                        ((), FeedbackData m o d e)
                        (SimulationStepOut o, FeedbackData m o d e)
    simulateParAux = proc (_, (sfs, ins)) -> do
      -- iterate agents in parallel
      (sfs', outs) <- runAgents -< (sfs, ins)

      -- SHUFFLING makes a difference regarding the order of the
      -- transactions, but ignoring shuffling for now

      -- TODO: problem is that runTransactions is currently not 
      -- keeping the ordering of the passed sfs/outs
      -- runTransactions -< 

      -- create next inputs and sfs (distribute messages and add/remove new/killed agents)
      (sfs'', ins') <- arrM (\(ins, outs, sfs') -> lift $ nextStep ins outs sfs') -< (ins, outs, sfs')
      -- create observable outputs
      let obs = observableAgents (map aiId ins) outs

      {-
      -- TODO: do NOT shuffle => must not make a difference
      -- NOTE: shuffling may seem strange in parallel but this will ensure random message-distribution when required
      (params'', sfsShuffled, insShuffled) = shuffleAgents params' sfs'' ins'
      -}

      t <- time -< ()

      returnA -< ((t, obs), (sfs'', ins'))

nextStep :: Monad m 
         => [AgentIn o d e]
         -> [AgentOut m o d e]
         -> [AgentCont m o d e]
         -> (ABSMonad m e) ([AgentCont m o d e], [AgentIn o d e])
nextStep oldAgentIns newAgentOuts asfs = do
    (asfs', newAgentIns) <- processAgents asfs oldAgentIns newAgentOuts

    -- NOTE: need to use oldAgentIns as each index corresponds to the agent in newAgentOuts
    let newAgentOutsWithAis = map (\(ai, ao) -> (aiId ai, ao)) (zip oldAgentIns newAgentOuts) 
        newAgentIns'        = distributeData newAgentIns newAgentOutsWithAis

    return (asfs', newAgentIns')
  where
    processAgents :: Monad m
                  => [AgentCont m o d e]
                  -> [AgentIn o d e]
                  -> [AgentOut m o d e]
                  -> (ABSMonad m e) ([AgentCont m o d e], [AgentIn o d e])
    processAgents asfs oldIs newOs = foldM handleAgent ([], []) asfsIsOs
      where
        asfsIsOs = zip3 asfs oldIs newOs

        handleAgent :: Monad m
                    => ([AgentCont m o d e], [AgentIn o d e])
                    -> (AgentCont m o d e, AgentIn o d e, AgentOut m o d e)
                    -> (ABSMonad m e) ([AgentCont m o d e], [AgentIn o d e])
        handleAgent acc a@(_, _, newOut) = do
            acc' <- handleCreateAgents newOut acc 
            return $ handleKillOrLiveAgent acc' a  

        handleKillOrLiveAgent :: ([AgentCont m o d e], [AgentIn o d e])
                              -> (AgentCont m o d e, AgentIn o d e, AgentOut m o d e)
                              -> ([AgentCont m o d e], [AgentIn o d e])
        handleKillOrLiveAgent acc@(asfsAcc, ainsAcc) (sf, oldIn, newOut)
            | killAgent = acc
            | otherwise = (sf : asfsAcc, newIn : ainsAcc) 
          where
            killAgent = isEvent $ aoKill newOut
            newIn = agentIn (aiId oldIn)

handleCreateAgents :: Monad m
                   => AgentOut m o d e
                   -> ([AgentCont m o d e], [AgentIn o d e])
                   -> (ABSMonad m e) ([AgentCont m o d e], [AgentIn o d e])
handleCreateAgents ao acc@(asfsAcc, ainsAcc) = do
    let newAgentDefs = aoCreate ao

    (newSfs, newAis) <- startingAgentM newAgentDefs

    if not $ null newAgentDefs 
      then return (asfsAcc ++ newSfs, ainsAcc ++ newAis)
      else return acc
  
distributeData :: [AgentIn o d e] 
               -> [(AgentId, AgentOut m o d e)] 
               -> [AgentIn o d e]
distributeData ains aouts = parMap rpar (distributeDataAux allData) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allData = collectAllData aouts

    distributeDataAux :: Map.Map AgentId [DataFlow d]
                      -> AgentIn o d e
                      -> AgentIn o d e
    distributeDataAux allData ain = ain'
      where
        receiverId = aiId ain
        ds = aiData ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverData = Map.lookup receiverId allData
        ds' = maybe ds (\receiverData -> receiverData ++ ds) mayReceiverData

        ain' = ain { aiData = ds' }

collectAllData :: [(AgentId, AgentOut m o d e)] -> Map.Map AgentId [DataFlow d]
collectAllData aos = foldr collectAllDataAux Map.empty aos
  where
    collectAllDataAux :: (AgentId, AgentOut m o d e)
                      -> Map.Map AgentId [DataFlow d]
                      -> Map.Map AgentId [DataFlow d]
    collectAllDataAux (senderId, ao) accData 
        | not $ null ds = foldr collectAllDataAuxAux accData ds
        | otherwise = accData
      where
        ds = aoData ao

        collectAllDataAuxAux :: DataFlow d
                             -> Map.Map AgentId [DataFlow d]
                             -> Map.Map AgentId [DataFlow d]
        collectAllDataAuxAux (receiverId, m) accData = accData'
          where
            d = (senderId, m)
            mayReceiverData = Map.lookup receiverId accData
            newData = maybe [d] (\receiverData -> d : receiverData) mayReceiverData

            -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
            accData' = seq newData (Map.insert receiverId newData accData)