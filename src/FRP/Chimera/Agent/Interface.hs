module FRP.Chimera.Agent.Interface 
  (
    AgentId
  , DataFlow
  , DataFilter
  
  , EventId
  , QueueItem (..)
  , ABSState (..)

  , ABSMonad

  , Agent
  , AgentCont
  , AgentTX

  , AgentDef (..)
  , AgentIn (..)
  , AgentOut (..)
  , AgentTXIn (..)
  , AgentTXOut (..)
  
  , absState
  
  , agentId
  , createAgent
  , kill
  , isDead
  , agentOut
  , agentOutObservable

  , onEvent
  , hasEvent
  , extractEvent

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

  , startingAgentInFromAgentDef
  , agentIn
  ) where

import           Data.List
import           Data.Maybe

import           Control.Monad.State.Strict
import qualified Data.PQueue.Min as PQ
import           FRP.BearRiver

type AgentId      = Int
type DataFlow d   = (AgentId, d)
type DataFilter d = DataFlow d -> Bool

type EventId      = Integer
data QueueItem e  = QueueItem AgentId e Time deriving Show
type EventQueue e = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

data ABSState e = ABSState
  { absNextId   :: AgentId
  , absEvtQueue :: EventQueue e

  , absTime     :: Time
  , absEvtIdx   :: Integer
  }

type ABSMonad m e = StateT (ABSState e) m

-- an agent is simply a SF with a generic computational context, which depends on the model
-- note that it is important that we do not fix m e.g. to StateT to allow an environment
-- or adding a RandT for allowing randomness but we leave this to the model implementer, otherwise
-- we would burden the API with details (type of the state in StateT, type of the RandomNumber generator 
-- in RandT) they may not need e.g. there are models which do not need a global read/write environment
-- or event don't use randonmness (e.g. SD emulation)
type AgentCont m o d e = SF (ABSMonad m e) (AgentIn o d e) (AgentOut m o d e)
type Agent m o d e     = AgentId -> (ABSMonad m e) (AgentCont m o d e)

type AgentTX m o d e = SF (ABSMonad m e) (AgentTXIn d) (AgentTXOut m o d e)

data AgentDef m o d e = AgentDef
  { adId       :: !AgentId
  , adBeh      :: Agent m o d e
  , adInitData :: ![DataFlow d]     -- AgentId identifies sender
  }

-- TODO: i think we can get rid of the aiId here because the Agent receives it already on start
data AgentIn o d e = AgentIn 
  { aiId              :: !AgentId
  , aiData            :: ![DataFlow d]     -- AgentId identifies sender
  
  , aiEvent           :: !(Event e)
  , aiRequestTx       :: !(Event (DataFlow d))

  , aiRec             :: !(Event [Maybe o])
  , aiRecInitAllowed  :: !Bool
  }

data AgentOut m o d e = AgentOut 
  { aoKill              :: !(Event ())
  , aoCreate            :: ![AgentDef m o d e]
  , aoData              :: ![DataFlow d]           -- AgentId identifies receiver

  , aoRequestTx         :: !(Event (DataFlow d, AgentTX m o d e))
  , aoAcceptTx          :: !(Event (d, AgentTX m o d e))

  , aoObservable        :: !(Maybe o)             -- OPTIONAL observable state

  , aoRec               :: !(Event ())
  , aoRecOthersAllowed  :: !Bool
  }

data AgentTXIn d = AgentTXIn
  { aiTxData   :: Maybe d
  , aiTxCommit :: Bool
  , aiTxAbort  :: Bool
  }

data AgentTXOut m o d e = AgentTXOut
  { aoTxData      :: Maybe d
  , aoTxCommit    :: Maybe (AgentOut m o d e, Maybe (AgentCont m o d e))
  , aoTxAbort     :: Bool
  }

-- TODO: need a type-class for joining the o type
-- which is itself the (>+<) operator =>
-- implement join type-class for AgentOut as well
(>+<) :: AgentOut m o d e
      -> AgentOut m o d e
      -> AgentOut m o d e
(>+<) ao0 ao1 = AgentOut
  { aoKill              = aoKill ao0 || aoKill ao1
  , aoCreate            = aoCreate ao0 ++ aoCreate ao1
  , aoData              = aoData ao0 ++ aoData ao1
  , aoRequestTx         :: !(Event (DataFlow d, AgentTX m o d e))
  , aoAcceptTx          :: !(Event (d, AgentTX m o d e))
  , aoObservable        = joinObs ao0 ao1
  , aoRec               :: !(Event ())
  , aoRecOthersAllowed  = aoRecOthersAllowed ao0 || aoRecOthersAllowed ao1
  }
  where
    joinObs :: AgentOut m o d e
            -> AgentOut m o d e
            -> Maybe o
    joinObs ao0 ao1
        -- TODO: isnt there a funciton which does this for us automatically? and we need only to apply the final function?
        | isJust o0 && isJust o1    = undefined
        | isJust o0 && isNothign o1 = o0
        | isNothing o0 && isJust o1 = o1
        | otherwise                 = Nothing
      where
        o0 = aoObservable ao0
        o1 = aoObservable ao1
-------------------------------------------------------------------------------
-- GENERAL 
-------------------------------------------------------------------------------
absState :: ABSState e
absState = ABSState
  { absNextId   = 0
  , absEvtQueue = PQ.empty

  , absTime     = 0
  , absEvtIdx   = 0
  }

agentId :: AgentIn o d e -> AgentId
agentId = aiId 

createAgent :: AgentDef m o d e -> AgentOut m o d e -> AgentOut m o d e
createAgent newDef ao = ao { aoCreate = newDef : aoCreate ao }

agentOut:: AgentOut m o d e
agentOut = agentOutAux Nothing

agentOutObservable :: o -> AgentOut m o d e
agentOutObservable o = agentOutAux (Just o)

kill :: AgentOut m o d e -> AgentOut m o d e
kill ao = ao { aoKill = Event () }

isDead :: AgentOut m o d e -> Bool
isDead = isEvent . aoKill


-------------------------------------------------------------------------------
-- EVENTS
-------------------------------------------------------------------------------
hasEvent :: AgentIn o d e -> Bool
hasEvent = isEvent . aiEvent 

extractEvent :: AgentIn o d e -> e
extractEvent = fromEvent . aiEvent

onEvent :: (e -> AgentOut m o d e -> AgentOut m o d e) 
        -> Event e 
        -> AgentOut m o d e 
        -> AgentOut m o d e
onEvent evtHdl evt ao = event ao (\e -> evtHdl e ao) evt

-------------------------------------------------------------------------------
-- MESSAGING / DATA-FLOW
-------------------------------------------------------------------------------
dataFlow :: DataFlow d -> AgentOut m o d e -> AgentOut m o d e
dataFlow d ao = ao { aoData = d : aoData ao }

dataFlowTo :: AgentId -> d -> AgentOut m o d e -> AgentOut m o d e
dataFlowTo aid msg = dataFlow (aid, msg)

dataFlows :: [DataFlow d] -> AgentOut m o d e ->  AgentOut m o d e
dataFlows msgs ao = foldr dataFlow ao msgs

broadcastDataFlow :: d -> [AgentId] -> AgentOut m o d e -> AgentOut m o d e
broadcastDataFlow d receiverIds = dataFlows datas
  where
    n = length receiverIds
    ds = replicate n d
    datas = zip receiverIds ds

hasDataFlow :: Eq d => d -> AgentIn o d e -> Bool
hasDataFlow d ai = Data.List.any ((==d) . snd) (aiData ai)

onDataFlow :: (DataFlow d -> acc -> acc) -> AgentIn o d e -> acc -> acc
onDataFlow dataHdl ai a = foldr dataHdl a ds
  where
    ds = aiData ai

onFilterDataFlow :: DataFilter d 
                 -> (DataFlow d -> acc -> acc) 
                 -> AgentIn o d e 
                 -> acc 
                 -> acc
onFilterDataFlow dataFilter dataHdl ai acc =
    foldr dataHdl acc dsFiltered
  where
    ds = aiData ai
    dsFiltered = filter dataFilter ds

onDataFlowFrom :: AgentId 
               -> (DataFlow d -> acc -> acc) 
               -> AgentIn o d e 
               -> acc 
               -> acc
onDataFlowFrom senderId datHdl ai acc = 
    onFilterDataFlow filterBySender datHdl ai acc
  where
    filterBySender :: DataFlow d -> Bool
    filterBySender (senderId', _) = senderId == senderId'

onDataFlowType :: (Eq d) 
               => d 
               -> (DataFlow d -> acc -> acc) 
               -> AgentIn o d e 
               -> acc 
               -> acc
onDataFlowType d datHdl ai acc = 
    onFilterDataFlow filterByType datHdl ai acc 
  where
    filterByType = (==d) . snd 

-------------------------------------------------------------------------------
-- OBSERVABLE STATE
-------------------------------------------------------------------------------
-- NOTE: assuming that state isJust
agentObservable :: AgentOut m o d e -> o
agentObservable = fromJust . aoObservable

-- NOTE: assuming that state isJust
updateAgentObservable :: (o -> o) -> AgentOut m o d e -> AgentOut m o d e
updateAgentObservable f ao = 
  ao { aoObservable = Just $ f $ fromJust $ aoObservable ao }

setAgentObservable :: o -> AgentOut m o d e -> AgentOut m o d e
setAgentObservable o ao = updateAgentObservable (const o) ao

-------------------------------------------------------------------------------
-- Transactions
-------------------------------------------------------------------------------
-- AgentIn TX related
isRequestTx :: AgentIn o d e -> Bool
isRequestTx = isEvent . aiRequestTx

requestTxData :: AgentIn o d e -> DataFlow d
requestTxData = fromEvent . aiRequestTx

requestTxIn :: AgentIn o d e -> Event (DataFlow d)
requestTxIn = aiRequestTx

-- AgentOut TX related
requestTx :: DataFlow d 
          -> AgentTX m o d e
          -> AgentOut m o d e 
          -> AgentOut m o d e
requestTx df txSf ao = ao { aoRequestTx = Event (df, txSf) }

acceptTX :: d 
         -> AgentTX m o d e
         -> AgentOut m o d e 
         -> AgentOut m o d e
acceptTX d txSf ao = ao { aoAcceptTx = Event (d, txSf) }

-- AgentTXOut related
agentTXOut :: AgentTXOut m o d e
agentTXOut = AgentTXOut
  {
    aoTxData    = Nothing
  , aoTxCommit  = Nothing
  , aoTxAbort   = False
  }

txDataOut :: d -> AgentTXOut m o d e -> AgentTXOut m o d e
txDataOut d aoTx = aoTx { aoTxData = Just d }

commitTx :: AgentOut m o d e 
         -> AgentTXOut m o d e 
         -> AgentTXOut m o d e
commitTx ao aoTx = aoTx { aoTxCommit = Just (ao, Nothing) }

commitTxWithCont :: AgentOut m o d e 
                 -> AgentCont m o d e
                 -> AgentTXOut m o d e 
                 -> AgentTXOut m o d e
commitTxWithCont ao sf aoTx = aoTx { aoTxCommit = Just (ao, Just sf) }

abortTx :: AgentTXOut m o d e -> AgentTXOut m o d e
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
agentRecursions :: AgentIn o d e -> Event [Maybe o]
agentRecursions = aiRec

recInitAllowed :: AgentIn o d e -> Bool
recInitAllowed = aiRecInitAllowed

allowsRecOthers :: AgentOut m o d e -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut m o d e -> AgentOut m o d e
recursive  allowOthers aout = 
  aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut m o d e -> AgentOut m o d e
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn o d e -> Bool
isRecursive ain = isEvent $ aiRec ain

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
startingAgentInFromAgentDef :: AgentDef m o d e -> AgentIn o d e
startingAgentInFromAgentDef ad = ai { aiData = adInitData ad }
  where
    ai = agentIn (adId ad) 

agentIn :: AgentId -> AgentIn o d e 
agentIn aid = AgentIn 
  { aiId             = aid
  , aiEvent          = NoEvent
  , aiData           = []
  , aiRequestTx      = NoEvent
  , aiRec            = NoEvent
  , aiRecInitAllowed = False
  }

-------------------------------------------------------------------------------
-- PRIVATE
-------------------------------------------------------------------------------
agentOutAux :: Maybe o -> AgentOut m o d e
agentOutAux o = AgentOut 
  { aoKill             = NoEvent
  , aoCreate           = []
  , aoData             = []
  , aoRequestTx        = NoEvent
  , aoAcceptTx         = NoEvent
  , aoObservable       = o
  , aoRec              = NoEvent
  , aoRecOthersAllowed = True
  }