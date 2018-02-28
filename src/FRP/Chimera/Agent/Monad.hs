module FRP.Chimera.Agent.Monad 
  ( 
    nextAgentIdM

  , onDataFlowM

  , scheduleEventM
  , unscheduleEventM

  , startingAgentM
  
  , ifThenElse
  , ifThenElseM
{-
    AgentMonadic

  , createAgentM
  , killM
  , isDeadM


  , dataFlowToM
  , dataFlowsM
  , broadcastDataFlowM
  , onDataFlowMState
  , onDataFlowM

  , bypassEnvironment

  , updateAgentObservableM
  , agentObservableM
  , setAgentObservableM
  , agentObservableFieldM

  , agentMonadic
-}
  ) where

import           Control.Monad.State.Strict
import qualified Data.PQueue.Min as PQ
import           FRP.BearRiver

import FRP.Chimera.Agent.Interface

-------------------------------------------------------------------------------
-- GENERAL
-------------------------------------------------------------------------------
nextAgentIdM :: Monad m 
             => (ABSMonad m e) AgentId
nextAgentIdM = do
  aid <- gets absNextId
  modify (\s -> s { absNextId = aid + 1})
  return aid

-------------------------------------------------------------------------------
-- MESSAGING / DATA-FLOW
-------------------------------------------------------------------------------
onDataFlowM :: Monad m
            => (acc -> DataFlow d-> m acc) 
            -> AgentIn o d e
            -> acc 
            -> m acc
onDataFlowM dfHdl ai acc = foldM dfHdl acc dfs
  where
    dfs = aiData ai

-------------------------------------------------------------------------------
-- SCHEDULING
-------------------------------------------------------------------------------
scheduleEventM :: Monad m
               => AgentId 
               -> e
               -> DTime
               -> (ABSMonad m e) EventId
scheduleEventM aid e dt = do
  q <- gets absEvtQueue
  t <- gets absTime
  eid <- gets absEvtIdx

  let qe = QueueItem aid e (t + dt)
  let q' = PQ.insert qe q

  modify (\s -> s { absEvtQueue = q', absEvtIdx = eid + 1 })

  return eid

-- TODO: implement
unscheduleEventM :: Monad m 
                 => EventId
                 -> (ABSMonad m e) Bool
unscheduleEventM _eid = undefined
-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
startingAgentM :: Monad m 
               => [AgentDef m o d e] 
               -> (ABSMonad m e) ([AgentCont m o d e], [AgentIn o d e])
startingAgentM adefs = do
    sfs <- mapM (\ad -> adBeh ad (adId ad)) adefs
    let ains = map startingAgentInFromAgentDef adefs
    return (sfs, ains)

-------------------------------------------------------------------------------
-- MONADIC UTILITIES
-------------------------------------------------------------------------------
ifThenElse :: Monad m 
           => Bool 
           -> m a 
           -> m a 
           -> m a
ifThenElse p trueAction falseAction 
  = if p then trueAction else falseAction

ifThenElseM :: Monad m 
            => m Bool 
            -> m a 
            -> m a 
            -> m a
ifThenElseM test trueAction falseAction 
  = test >>= \t -> if t then trueAction else falseAction



{-
-- TODO: this is not general yet: need to run the whole monad stack
type AgentMonadic m o d = Double -> AgentIn o d -> State (AgentOut m o d) ()

-------------------------------------------------------------------------------
-- GENERAL
-------------------------------------------------------------------------------
createAgentM :: MonadState (AgentOut m o d) m
             => AgentDef m o d
             -> StateT (AgentOut m o d) m ()
createAgentM newDef = state (\ao -> ((), createAgent newDef ao))

killM :: MonadState (AgentOut m o d) m
      => StateT (AgentOut m o d) m ()
killM = state (\ao -> ((), kill ao))

isDeadM :: MonadState (AgentOut m o d) m
        => StateT (AgentOut m o d) m Bool
isDeadM = state (\ao -> (isDead ao, ao))


dataFlowM :: Monad m
          => DataFlow d
          -> StateT (AgentOut m o d) m ()
dataFlowM df = state (\ao -> ((), dataFlow df ao))

dataFlowToM :: Monad m
            => AgentId
            -> d 
            -> StateT (AgentOut m o d) m ()
dataFlowToM receiver d = state (\ao -> ((), dataFlowTo receiver d ao))

dataFlowsM :: Monad m
           => [DataFlow d] 
           -> StateT (AgentOut m o d) m ()
dataFlowsM dfs = state (\ao -> ((), dataFlows dfs ao))

broadcastDataFlowM :: Monad m
                   => d 
                   -> [AgentId]
                   -> StateT (AgentOut m o d) m ()
broadcastDataFlowM d receiverIds = state (broadcastDataFlowMAux d)
  where
    broadcastDataFlowMAux :: d -> AgentOut m o d -> ((), AgentOut m o d)
    broadcastDataFlowMAux d ao = ((), dataFlows dfs ao)
      where
        n = length receiverIds
        ds = replicate n d
        dfs = zip receiverIds ds

onDataFlowMState :: MonadState acc m
                 => (DataFlow d-> m ()) 
                 -> AgentIn o d
                 -> m ()
onDataFlowMState dfHdl ai = onDataFlowM (\_ df -> dfHdl df) ai ()

 
-------------------------------------------------------------------------------
-- OBSERVABLE STATE
-------------------------------------------------------------------------------
-- NOTE: assuming state isJust
agentObservableM :: Monad m => StateT (AgentOut m o d) m o
agentObservableM = state (\ao -> (agentObservable ao, ao))

setAgentObservableM :: Monad m
                    => o 
                    -> StateT (AgentOut m o d) m ()
setAgentObservableM o = state (\ao -> ((), setAgentObservable o ao))

-- NOTE: assuming state isJust
updateAgentObservableM :: Monad m
                       => (o -> o)
                       -> StateT (AgentOut m o d) m ()
updateAgentObservableM f = state (updateAgentObservableMAux f)
  where
    updateAgentObservableMAux :: (o -> o) 
                              -> AgentOut m o d 
                              -> ((), AgentOut m o d)
    updateAgentObservableMAux f ao = ((), updateAgentObservable f ao)

-- NOTE: assuming state isJust
agentObservableFieldM :: Monad m
                      => (o -> t)
                      -> StateT (AgentOut m o d) m t
agentObservableFieldM f = state (agentObservableFieldMAux f)
  where
    agentObservableFieldMAux :: (o -> t) 
                             -> AgentOut m o d
                             -> (t, AgentOut m o d)
    agentObservableFieldMAux f ao = (f o, ao)
      where
        o = agentObservable ao

{-
-------------------------------------------------------------------------------
-- CONVERSATIONS
-------------------------------------------------------------------------------
conversationM :: Monad m
              => AgentData d
              -> AgentConversationSender m o d e
              -> StateT (AgentOut m o d) m ()
conversationM d replyHdl = state (\ao -> ((), conversation d replyHdl ao))

conversationEndM :: Monad m => StateT (AgentOut m o d) m ()
conversationEndM = state (\ao -> ((), conversationEnd ao))


conversationReplyMonadicRunner :: MonadState (AgentOut m o d) m
                               => (Maybe (AgentData d) -> e -> m e)
                               -> AgentConversationSender m o d e
conversationReplyMonadicRunner replyAction ao e mayReply = (ao', e')
  where
    (e', ao') = runState (replyAction mayReply e) ao

conversationIgnoreEnvReplyMonadicRunner :: MonadState (AgentOut m o d) m
                                        => (Maybe (AgentData d) -> m ()) 
                                        -> AgentConversationSender m o d e
conversationIgnoreEnvReplyMonadicRunner replyAction ao e mayReply = (ao', e)
  where
    (_, ao') = runState (replyAction mayReply) ao

-- NOTE: when ignoring the reply it makes also sense to bypass the environment
conversationIgnoreReplyMonadicRunner :: MonadState (AgentOut m o d) m
                                     => m () 
                                     -> AgentConversationSender m o d e
conversationIgnoreReplyMonadicRunner replyAction ao e  _ = (ao', e)
  where
    (_, ao') = runState replyAction ao

-- NOTE: for the case one does not want to bypass the environment
conversationIgnoreReplyMonadicRunner' :: MonadState (AgentOut m o d) m
                                      => (e -> m e) 
                                      -> AgentConversationSender m o d e
conversationIgnoreReplyMonadicRunner' replyAction ao e _ = (ao', e')
  where
    (e', ao') = runState (replyAction e) ao
-}

bypassEnvironment :: Monad m
                  => StateT (AgentOut m o d) m () 
                  -> e 
                  -> StateT (AgentOut m o d) m e
bypassEnvironment a e = a >> return e

-------------------------------------------------------------------------------
-- MONADIC WRAPPERS
-------------------------------------------------------------------------------
-- TODO: this is not correct yet
agentMonadic :: Monad m 
             => AgentMonadic m o d
             -> AgentOut m o d 
             -> Agent m o d
agentMonadic f ao = proc ain -> do
  age <- time -< ()
  let (_, ao') = runState (f age ain) ao  
  returnA -< ao'
-}