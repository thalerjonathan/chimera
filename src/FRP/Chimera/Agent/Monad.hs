{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Agent.Monad 
  (
    AgentMonadic
  , AgentMonadicReadEnv
  , AgentMonadicNoEnv

  , createAgentM
  , killM
  , isDeadM

  , dataFlowM
  , dataFlowToM
  , dataFlowsM
  , broadcastDataFlowM
  , onDataFlowMState
  , onDataFlowM

  , conversationM
  , conversationEndM
  -- , conversationReplyMonadicRunner
  -- , conversationIgnoreEnvReplyMonadicRunner
  -- , conversationIgnoreReplyMonadicRunner
  -- , conversationIgnoreReplyMonadicRunner'

  , bypassEnvironment

  , updateAgentObservableM
  , agentObservableM
  , setAgentObservableM
  , agentObservableFieldM

  , agentMonadic
  , agentMonadicReadEnv
  , agentMonadicIgnoreEnv

  , ifThenElse
  , ifThenElseM
  ) where

import Control.Monad
--import Control.Monad.State
import Control.Monad.State.Strict
import FRP.BearRiver

import FRP.Chimera.Agent.Interface

type AgentMonadic m o d e          = e -> Double -> AgentIn o d e -> State (AgentOut m o d e) e
type AgentMonadicReadEnv m o d e   = e -> Double -> AgentIn o d e -> State (AgentOut m o d e) ()
type AgentMonadicNoEnv m o d e     = Double -> AgentIn o d e -> State (AgentOut m o d e) ()

-------------------------------------------------------------------------------
-- GENERAL
-------------------------------------------------------------------------------
createAgentM :: MonadState (AgentOut m o d e) m
             => AgentDef m o d e
             -> StateT (AgentOut m o d e) m ()
createAgentM newDef = state (\ao -> ((), createAgent newDef ao))

killM :: MonadState (AgentOut m o d e) m
      => StateT (AgentOut m o d e) m ()
killM = state (\ao -> ((), kill ao))

isDeadM :: MonadState (AgentOut m o d e) m
        => StateT (AgentOut m o d e) m Bool
isDeadM = state (\ao -> (isDead ao, ao))

-------------------------------------------------------------------------------
-- MESSAGING / DATA-FLOW
-------------------------------------------------------------------------------
dataFlowM :: Monad m
          => AgentData d 
          -> StateT (AgentOut m o d e) m ()
dataFlowM df = state (\ao -> ((), dataFlow df ao))

dataFlowToM :: Monad m
            => AgentId
            -> d 
            -> StateT (AgentOut m o d e) m ()
dataFlowToM receiver d = state (\ao -> ((), dataFlowTo receiver d ao))

dataFlowsM :: Monad m
           => [AgentData d] 
           -> StateT (AgentOut m o d e) m ()
dataFlowsM dfs = state (\ao -> ((), dataFlows dfs ao))

broadcastDataFlowM :: Monad m
                   => d 
                   -> [AgentId]
                   -> StateT (AgentOut m o d e) m ()
broadcastDataFlowM d receiverIds = state (broadcastDataFlowMAux d)
  where
    broadcastDataFlowMAux :: d -> AgentOut m o d e -> ((), AgentOut m o d e)
    broadcastDataFlowMAux d ao = ((), dataFlows dfs ao)
      where
        n = length receiverIds
        ds = replicate n d
        dfs = zip receiverIds ds

onDataFlowMState :: MonadState acc m
                 => (AgentData d -> m ()) 
                 -> AgentIn o d e
                 -> m ()
onDataFlowMState dfHdl ai = onDataFlowM (\_ df -> dfHdl df) ai ()

onDataFlowM :: Monad m
            => (acc -> AgentData d -> m acc) 
            -> AgentIn o d e
            -> acc 
            -> m acc
onDataFlowM dfHdl ai acc = foldM dfHdl acc dfs
  where
    dfs = aiData ai
   
-------------------------------------------------------------------------------
-- OBSERVABLE STATE
-------------------------------------------------------------------------------
-- NOTE: assuming state isJust
agentObservableM :: Monad m => StateT (AgentOut m o d e) m o
agentObservableM = state (\ao -> (agentObservable ao, ao))

setAgentObservableM :: Monad m
                    => o 
                    -> StateT (AgentOut m o d e) m ()
setAgentObservableM o = state (\ao -> ((), setAgentObservable o ao))

-- NOTE: assuming state isJust
updateAgentObservableM :: Monad m
                       => (o -> o)
                       -> StateT (AgentOut m o d e) m ()
updateAgentObservableM f = state (updateAgentObservableMAux f)
  where
    updateAgentObservableMAux :: (o -> o) 
                              -> AgentOut m o d e 
                              -> ((), AgentOut m o d e)
    updateAgentObservableMAux f ao = ((), updateAgentObservable f ao)

-- NOTE: assuming state isJust
agentObservableFieldM :: Monad m
                      => (o -> t)
                      -> StateT (AgentOut m o d e) m t
agentObservableFieldM f = state (agentObservableFieldMAux f)
  where
    agentObservableFieldMAux :: (o -> t) 
                             -> AgentOut m o d e
                             -> (t, AgentOut m o d e)
    agentObservableFieldMAux f ao = (f o, ao)
      where
        o = agentObservable ao

-------------------------------------------------------------------------------
-- CONVERSATIONS
-------------------------------------------------------------------------------
conversationM :: Monad m
              => AgentData d
              -> AgentConversationSender m o d e
              -> StateT (AgentOut m o d e) m ()
conversationM d replyHdl = state (\ao -> ((), conversation d replyHdl ao))

conversationEndM :: Monad m => StateT (AgentOut m o d e) m ()
conversationEndM = state (\ao -> ((), conversationEnd ao))

{-
conversationReplyMonadicRunner :: MonadState (AgentOut m o d e) m
                               => (Maybe (AgentData d) -> e -> m e)
                               -> AgentConversationSender m o d e
conversationReplyMonadicRunner replyAction ao e mayReply = (ao', e')
  where
    (e', ao') = runState (replyAction mayReply e) ao

conversationIgnoreEnvReplyMonadicRunner :: MonadState (AgentOut m o d e) m
                                        => (Maybe (AgentData d) -> m ()) 
                                        -> AgentConversationSender m o d e
conversationIgnoreEnvReplyMonadicRunner replyAction ao e mayReply = (ao', e)
  where
    (_, ao') = runState (replyAction mayReply) ao

-- NOTE: when ignoring the reply it makes also sense to bypass the environment
conversationIgnoreReplyMonadicRunner :: MonadState (AgentOut m o d e) m
                                     => m () 
                                     -> AgentConversationSender m o d e
conversationIgnoreReplyMonadicRunner replyAction ao e  _ = (ao', e)
  where
    (_, ao') = runState replyAction ao

-- NOTE: for the case one does not want to bypass the environment
conversationIgnoreReplyMonadicRunner' :: MonadState (AgentOut m o d e) m
                                      => (e -> m e) 
                                      -> AgentConversationSender m o d e
conversationIgnoreReplyMonadicRunner' replyAction ao e _ = (ao', e')
  where
    (e', ao') = runState (replyAction e) ao
-}

bypassEnvironment :: Monad m
                  => StateT (AgentOut m o d e) m () 
                  -> e 
                  -> StateT (AgentOut m o d e) m e
bypassEnvironment a e = a >> return e

-------------------------------------------------------------------------------
-- MONADIC WRAPPERS
-------------------------------------------------------------------------------
agentMonadic :: Monad m 
             => AgentMonadic m o d e 
             -> AgentOut m o d e 
             -> Agent m o d e
agentMonadic f ao = proc (ain, e) -> do
  age <- time -< ()
  let (e', _ao') = runState (f e age ain) ao
  -- TODO: put ao' using state-monad
  returnA -< e'

agentMonadicReadEnv :: Monad m 
                    => AgentMonadicReadEnv m o d e 
                    -> AgentOut m o d e 
                    -> Agent m o d e
agentMonadicReadEnv f ao = proc (ain, e) -> do
  age <- time -< ()
  let _ao' = execState (f e age ain) ao
  -- TODO: put ao' using state-monad
  returnA -< e

agentMonadicIgnoreEnv :: Monad m 
                      => AgentMonadicNoEnv m o d e 
                      -> AgentOut m o d e 
                      -> Agent m o d e
agentMonadicIgnoreEnv f ao = proc (ain, e) -> do
  age <- time -< ()
  let _ao' = execState (f age ain) ao
  -- TODO: put ao' using state-monad
  returnA -< e

-------------------------------------------------------------------------------
-- MONADIC UTILITIES
-------------------------------------------------------------------------------
ifThenElse :: Monad m => Bool -> m a -> m a -> m a
ifThenElse p trueAction falseAction = if p then trueAction else falseAction

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM test trueAction falseAction = test >>= \t -> if t then trueAction else falseAction
