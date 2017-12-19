{-# LANGUAGE Arrows #-}
module FRP.Chimera.Agent.Interface 
  (
    AgentId
  , AgentData
  , DataFilter

  , Agent
  , AgentRandom
  , AgentTX

  , AgentDef (..)
  , AgentIn (..)
  , AgentOut (..)
  , AgentTXIn (..)
  , AgentTXOut (..)
  
  , agentId
  , createAgent
  , kill
  , isDead
  , agentOut
  , agentOutObservable
  , nextAgentId

  , onStart
  , onEvent

  , dataFlow
  , dataFlowTo
  , dataFlows
  , broadcastDataFlow
  , hasDataFlow
  , onDataFlow
  , onFilterDataFlow
  , onDataFlowFrom
  , onDataFlowType

  , isRequestTx
  , requestTxData
  , requestTxIn
  , requestTx
  , acceptTX
  , agentTXOut
  , txDataOut
  , commitTx
  , commitTxWithCont
  , abortTx
  , txDataIn
  , hasTxDataIn
  , isCommitTX
  , isAbortTX

  , agentObservable
  , updateAgentObservable
  , setAgentObservable

  , recInitAllowed
  , allowsRecOthers
  , recursive
  , unrecursive
  , isRecursive
  , agentRecursions

  , startingAgent
  , startingAgentIn
  , startingAgentInFromAgentDef
  , agentIn
  ) where

import Data.List
import Data.Maybe

import Control.Concurrent.STM.TVar
import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver

import FRP.Chimera.Simulation.Internal

type AgentId              = Int
type AgentData d          = (AgentId, d)
type DataFilter d         = AgentData d -> Bool

-- TODO: should we instead return the observable explicitly as a (Maybe o) instead
-- of having it inside the agentout?
type Agent m o d          = SF (StateT (AgentOut m o d) m) (AgentIn o d) ()
type AgentRandom g o d    = Agent (Rand g) o d 
-- TODO: should we prevent envrionment-modification in TX-functions? 
-- can achieve this by replacing m by Identity monad
type AgentTX m o d        = SF m (AgentTXIn d) (AgentTXOut m o d)

data AgentDef m o d = AgentDef
  { adId           :: !AgentId
  , adBeh          :: Agent m o d
  , adInitData     :: ![AgentData d]     -- AgentId identifies sender
  }

-- TODO: remove IdGen, it is a pain in the ass
data AgentIn o d = AgentIn 
  { aiId              :: !AgentId
  , aiIdGen           :: !(TVar Int)
  , aiStart           :: !(Event ())
  , aiData            :: ![AgentData d]     -- AgentId identifies sender
  
  , aiRequestTx       :: !(Event (AgentData d))

  , aiRec             :: !(Event [(Maybe o)])
  , aiRecInitAllowed  :: !Bool
  }

data AgentOut m o d = AgentOut 
  { aoKill              :: !(Event ())
  , aoCreate            :: ![AgentDef m o d]
  , aoData              :: ![AgentData d]           -- AgentId identifies receiver

  , aoRequestTx         :: !(Event (AgentData d, AgentTX m o d))
  , aoAcceptTx          :: !(Event (d, AgentTX m o d))

  , aoObservable        :: !(Maybe o)             -- OPTIONAL observable state

  , aoRec               :: !(Event ())
  , aoRecOthersAllowed  :: !Bool
  }

data AgentTXIn d = AgentTXIn
  { aiTxData      :: Maybe d
  , aiTxCommit    :: Bool
  , aiTxAbort     :: Bool
  }

-- note that commit returns a agent-sf. this allows
-- to pass state which has changed in the TX to the agent-sf.
-- If we ommit this agent-sf and simply run the existing one 
-- then we couldn't directly pass new state but time-dependent
-- accumulator functions (e.g. time, integral) wouldnt get
-- reset. Maybe we make the function optional using a maybe
-- and if it exists then we use the new one, otherwise run
-- the old one.
-- abort is only a boolean flag and if set to true the
-- old SF and old AgentOut will be restored but with
-- TX begin set to Nothing
data AgentTXOut m o d = AgentTXOut
  { aoTxData      :: Maybe d
  , aoTxCommit    :: Maybe (AgentOut m o d, Maybe (Agent m o d))
  , aoTxAbort     :: Bool
  }

-------------------------------------------------------------------------------
-- GENERAL 
-------------------------------------------------------------------------------
agentId :: AgentIn o d -> AgentId
agentId = aiId 

createAgent :: AgentDef m o d -> AgentOut m o d -> AgentOut m o d
createAgent newDef ao = ao { aoCreate = newDef : aoCreate ao }

agentOut:: AgentOut m o d
agentOut = agentOutAux Nothing

agentOutObservable :: o -> AgentOut m o d
agentOutObservable o = agentOutAux (Just o)

nextAgentId :: AgentIn o d -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut m o d -> AgentOut m o d
kill ao = ao { aoKill = Event () }

isDead :: AgentOut m o d -> Bool
isDead = isEvent . aoKill

-------------------------------------------------------------------------------
-- EVENTS
-------------------------------------------------------------------------------
onStart :: (AgentOut m o d -> AgentOut m o d) 
        -> AgentIn o d 
        -> AgentOut m o d 
        -> AgentOut m o d
onStart evtHdl ai ao = onEvent evtHdl startEvt ao
  where
    startEvt = aiStart ai

onEvent :: (AgentOut m o d -> AgentOut m o d) 
        -> Event () 
        -> AgentOut m o d 
        -> AgentOut m o d
onEvent evtHdl evt ao = event ao (\_ -> evtHdl ao) evt

-------------------------------------------------------------------------------
-- MESSAGING / DATA-FLOW
-------------------------------------------------------------------------------
dataFlow :: AgentData d -> AgentOut m o d -> AgentOut m o d
dataFlow d ao = ao { aoData = d : aoData ao }

dataFlowTo :: AgentId -> d -> AgentOut m o d -> AgentOut m o d
dataFlowTo aid msg ao = dataFlow (aid, msg) ao

dataFlows :: [AgentData d] -> AgentOut m o d ->  AgentOut m o d
dataFlows msgs ao = foldr dataFlow ao msgs

broadcastDataFlow :: d -> [AgentId] -> AgentOut m o d -> AgentOut m o d
broadcastDataFlow d receiverIds ao = dataFlows datas ao
  where
    n = length receiverIds
    ds = replicate n d
    datas = zip receiverIds ds

hasDataFlow :: Eq d => d -> AgentIn o d -> Bool
hasDataFlow d ai = Data.List.any ((==d) . snd) (aiData ai)

onDataFlow :: (AgentData d -> acc -> acc) -> AgentIn o d -> acc -> acc
onDataFlow dataHdl ai a = foldr (\d acc'-> dataHdl d acc') a ds
  where
    ds = aiData ai

onFilterDataFlow :: DataFilter d 
                 -> (AgentData d -> acc -> acc) 
                 -> AgentIn o d 
                 -> acc 
                 -> acc
onFilterDataFlow dataFilter dataHdl ai acc =
    foldr (\d acc'-> dataHdl d acc') acc dsFiltered
  where
    ds = aiData ai
    dsFiltered = filter dataFilter ds

onDataFlowFrom :: AgentId 
               -> (AgentData d -> acc -> acc) 
               -> AgentIn o d 
               -> acc 
               -> acc
onDataFlowFrom senderId datHdl ai acc = 
    onFilterDataFlow filterBySender datHdl ai acc
  where
    filterBySender = (\(senderId', _) -> senderId == senderId' )

onDataFlowType :: (Eq d) 
               => d 
               -> (AgentData d -> acc -> acc) 
               -> AgentIn o d 
               -> acc 
               -> acc
onDataFlowType d datHdl ai acc = onFilterDataFlow filterByType datHdl ai acc
  where
    filterByType = (==d) . snd 

-------------------------------------------------------------------------------
-- OBSERVABLE STATE
-------------------------------------------------------------------------------
-- NOTE: assuming that state isJust
agentObservable :: AgentOut m o d -> o
agentObservable = fromJust . aoObservable

-- NOTE: assuming that state isJust
updateAgentObservable :: (o -> o) -> AgentOut m o d -> AgentOut m o d
updateAgentObservable f ao = 
  ao { aoObservable = Just $ f $ fromJust $ aoObservable ao }

setAgentObservable :: o -> AgentOut m o d -> AgentOut m o d
setAgentObservable o ao = updateAgentObservable (const o) ao

-------------------------------------------------------------------------------
-- Transactions
-------------------------------------------------------------------------------
-- AgentIn TX related
isRequestTx :: AgentIn o d -> Bool
isRequestTx = isEvent . aiRequestTx

requestTxData :: AgentIn o d -> AgentData d
requestTxData = fromEvent . aiRequestTx

requestTxIn :: AgentIn o d -> Event (AgentData d)
requestTxIn = aiRequestTx

-- AgentOut TX related
requestTx :: AgentData d 
          -> AgentTX m o d
          -> AgentOut m o d 
          -> AgentOut m o d
requestTx df txSf ao = ao { aoRequestTx = Event (df, txSf) }

acceptTX :: d 
         -> AgentTX m o d
         -> AgentOut m o d 
         -> AgentOut m o d
acceptTX d txSf ao = ao { aoAcceptTx = Event (d, txSf) }

-- AgentTXOut related
agentTXOut :: AgentTXOut m o d
agentTXOut = AgentTXOut
  {
    aoTxData    = Nothing
  , aoTxCommit  = Nothing
  , aoTxAbort   = False
  }

txDataOut :: d -> AgentTXOut m o d -> AgentTXOut m o d
txDataOut d aoTx = aoTx { aoTxData = Just d }

commitTx :: AgentOut m o d 
         -> AgentTXOut m o d 
         -> AgentTXOut m o d
commitTx ao aoTx = aoTx { aoTxCommit = Just (ao, Nothing) }

commitTxWithCont :: AgentOut m o d 
                 -> Agent m o d
                 -> AgentTXOut m o d 
                 -> AgentTXOut m o d
commitTxWithCont ao sf aoTx = aoTx { aoTxCommit = Just (ao, Just sf) }

abortTx :: AgentTXOut m o d -> AgentTXOut m o d
abortTx aoTx = aoTx { aoTxAbort = True}

-- AgentTXIn related
txDataIn :: AgentTXIn d -> d
txDataIn = fromJust . aiTxData

hasTxDataIn :: AgentTXIn d -> Bool
hasTxDataIn = isJust . aiTxData

isCommitTX :: AgentTXIn d -> Bool
isCommitTX = aiTxCommit

isAbortTX :: AgentTXIn d -> Bool
isAbortTX = aiTxAbort
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- RECURSION
-------------------------------------------------------------------------------
agentRecursions :: AgentIn o d -> Event [Maybe o]
agentRecursions = aiRec

recInitAllowed :: AgentIn o d -> Bool
recInitAllowed = aiRecInitAllowed

allowsRecOthers :: AgentOut m o d -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut m o d -> AgentOut m o d
recursive  allowOthers aout = 
  aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut m o d -> AgentOut m o d
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn o d -> Bool
isRecursive ain = isEvent $ aiRec ain

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
startingAgentIn :: [AgentDef m o d] -> TVar Int -> [AgentIn o d]
startingAgentIn adefs idGen = map (startingAgentInFromAgentDef idGen) adefs

startingAgent :: [AgentDef m o d] 
              -> TVar Int 
              -> ([Agent m o d], [AgentIn o d])
startingAgent adefs idGen = (sfs, ains)
  where
    ains = startingAgentIn adefs idGen
    sfs = map adBeh adefs 

startingAgentInFromAgentDef :: TVar Int -> AgentDef m o d -> AgentIn o d
startingAgentInFromAgentDef idGen ad = 
  AgentIn { aiId              = adId ad
          , aiData            = adInitData ad
          , aiRequestTx       = NoEvent
          , aiStart           = Event ()
          , aiRec             = NoEvent
          , aiRecInitAllowed  = True
          , aiIdGen           = idGen 
          }

agentIn :: AgentId -> TVar Int -> AgentIn o d 
agentIn aid idGen = AgentIn 
  { aiId              = aid
  , aiIdGen           = idGen
  , aiStart           = NoEvent
  , aiData            = []
  , aiRequestTx       = NoEvent
  , aiRec             = NoEvent
  , aiRecInitAllowed  = False
  }

-------------------------------------------------------------------------------
-- PRIVATE
-------------------------------------------------------------------------------
agentOutAux :: Maybe o -> AgentOut m o d
agentOutAux o = 
  AgentOut {  aoKill              = NoEvent
            , aoCreate            = []
            , aoData              = []
            , aoRequestTx         = NoEvent
            , aoAcceptTx          = NoEvent
            , aoObservable        = o
            , aoRec               = NoEvent
            , aoRecOthersAllowed  = True
            }