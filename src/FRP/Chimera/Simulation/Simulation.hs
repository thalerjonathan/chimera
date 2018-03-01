module FRP.Chimera.Simulation.Simulation 
  (
    AgentObservableAggregator

  --, simulateIOInit

  , simulate
  , simulateEvents
  , simulateTime
  --, simulateTimeDeltas
  --, simulateAggregateTimeDeltas
  -- , simulateAggregateTime

  -- , simulateDebug
  -- , simulateDebugInternal
  ) where

import           Data.Maybe

import           Control.Monad.Trans.MSF.Reader
import           Control.Monad.State.Strict
--import           Control.Parallel.Strategies
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQ
import           FRP.BearRiver

import           FRP.Chimera.Agent.Interface
import           FRP.Chimera.Agent.Monad 
import           FRP.Chimera.Simulation.Common
import           FRP.Chimera.Simulation.ParIteration

type AgentObservableAggregator o a  = SimulationStepOut o -> a

-------------------------------------------------------------------------------
-- TIME-DRIVEN SIMULATION
-------------------------------------------------------------------------------
simulateTime :: Monad m
             => [AgentDef m o d e]
             -> DTime
             -> Time
             -> m [SimulationStepOut o]
simulateTime adefs dt t = do
    let steps  = floor $ t / dt
        ticks  = replicate steps ()
    
    ((asfs, ais), s') <- runStateT (startingAgentM adefs) absState

    let aossReader = embed (simulatePar asfs ais) ticks
        aossState  = runReaderT aossReader dt
        aossM      = evalStateT aossState s'

    aossM

-------------------------------------------------------------------------------
-- EVENT-DRIVEN SIMULATION
-------------------------------------------------------------------------------
simulateEvents :: Monad m
               => [AgentDef m o d e]
               -> DTime
               -> Time
               -> m a
               -> m (Time, Integer, [(Time, a)])
simulateEvents adefs tSampling tLimit samplingFunc = do
    ((asfs, ais), abs0) <- runStateT (startingAgentM adefs) absState
    
    let asMap = Prelude.foldr (\(ai, asf) acc -> Map.insert (aiId ai) asf acc) Map.empty (Prelude.zip ais asfs)
    (samples, absFinal) <- runStateT (stepClock 0 asMap samplingFunc []) abs0
    
    let finalTime   = absTime absFinal
    let finalEvtCnt = absEvtIdx absFinal

    return (finalTime, finalEvtCnt, reverse samples)

  where
    stepClock :: Monad m 
              => Time
              -> Map.Map AgentId (AgentCont m o d e)
              -> m a
              -> [(Time, a)] 
              -> (ABSMonad m e) [(Time, a)]
    stepClock ts asMap samplingFunc acc = do
      q <- gets absEvtQueue

      -- TODO: run in Maybe?

      let mayHead = PQ.getMin q
      if isNothing mayHead
        then return acc
        else do
          ec <- gets absEvtIdx
          t  <- gets absTime

          let _qi@(QueueItem aid e t') = fromJust mayHead
          let q' = PQ.drop 1 q

          -- modify time and changed queue before running the process
          -- because the process might change the queue
          modify (\s -> s { 
            absEvtQueue = q' 
          , absTime     = t'
          , absEvtIdx   = ec + 1
          })

          let ac = fromJust $ Map.lookup aid asMap
          let ai = (agentIn aid) { aiEvent = Event e }
          let localDt = t' - t

          let acReader = unMSF ac ai
          (_, ac') <- runReaderT acReader localDt -- event-driven simulation completely ignores agentout for now
         
          let asMap' = Map.insert aid ac' asMap

          s <- lift samplingFunc
          let (acc', ts') = if ts >= tSampling
                then ((t', s) : acc, 0)
                else (acc, ts + (t' - t))

          if t' < tLimit
            then stepClock ts' asMap' samplingFunc acc'
            else return acc'

-------------------------------------------------------------------------------
-- EVENT- & TIME-DRIVEN SIMULATION combined
-------------------------------------------------------------------------------
simulate :: Monad m
         => [AgentDef m o d e]
         -> DTime
         -> Time
         -> m a
         -> m (Time, Integer, [(Time, [AgentObservable o], a)])
simulate adefs dt tEnd samplingFunc = do
    ((asfs, ais), abs0) <- runStateT (startingAgentM adefs) absState
    
    let asMap = Prelude.foldr (\(ai, asf) acc -> Map.insert (aiId ai) (0, ai, asf) acc) Map.empty (Prelude.zip ais asfs)
    (samples, absFinal) <- runStateT (stepClock 0 asMap samplingFunc []) abs0
    
    let finalTime   = absTime absFinal
    let finalEvtCnt = absEvtIdx absFinal

    return (finalTime, finalEvtCnt, reverse samples)

  where
    stepClock :: Monad m 
              => Time
              -> Map.Map AgentId (Time, AgentIn o d e, AgentCont m o d e)
              -> m a
              -> [(Time, [AgentObservable o], a)]
              -> (ABSMonad m e) [(Time, [AgentObservable o], a)]
    stepClock t asMap samplingFunc acc = do
      let t' = t + dt

      -- note thats this can result in an infinte loop if always events are scheduled with dt=0
      asMap' <- runEventsUntil t' asMap

      -- TODO run all agents as in ParIteration with dt and collect aos
      let aoss = []

      -- in combined mode, the surrounding monad m is sampled every dt
      s <- lift samplingFunc

      let acc' = (t', aoss, s) : acc

      if t' < tEnd
        then stepClock t' asMap' samplingFunc acc'
        else return acc'

    runEventsUntil :: Monad m 
                   => Time
                   -> Map.Map AgentId (Time, AgentIn o d e, AgentCont m o d e)
                   -> (ABSMonad m e) (Map.Map AgentId (Time, AgentIn o d e, AgentCont m o d e))
    runEventsUntil tLimit asMap = do
      q <- gets absEvtQueue

      -- TODO use maybe?

      let mayHead = PQ.getMin q
      if isNothing mayHead
        then return asMap
        else do
          let (QueueItem aid e eventTime) = fromJust mayHead

          -- if the current event happens before the next time-driven sampling, then execute the event 
          -- note that we need to repeat this until all events with eventTime < t' 
          -- are processed or no more events in queue, then we can go on with the next dt step
          -- note that we need to use the correct dt since the agents (not the global clock) last update

          -- if the eventTime is larger then the time until to run (t) all subsequent events
          -- happen after t as well, at this point we are done
          if eventTime > tLimit
            then return asMap
            else do
              let q' = PQ.drop 1 q

              let (at, ai, ac) = fromJust $ Map.lookup aid asMap

              t <- gets absTime

              -- modify time and changed queue before running the process
              -- because the process might change the queue
              modify (\s -> s { 
                absEvtQueue = q'
              , absTime     = eventTime
              , absEvtIdx   = absEvtIdx s + 1
              })

              -- constructing new AgentIn just for event, ignoring ai, is used for
              -- time-driven approach
              let aiEvent = (agentIn aid) { aiEvent = Event e }
              let localDt = t - at

              let acReader = unMSF ac aiEvent
              (_, ac') <- runReaderT acReader localDt
            
              let asMap' = Map.insert aid (eventTime, ai, ac') asMap

              runEventsUntil tLimit asMap'

{-
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
            -}
{-
-------------------------------------------------------------------------------
-- RUNNING SIMULATION FROM AN OUTER LOOP
-------------------------------------------------------------------------------
simulateIOInit :: [AgentDef m o d]
                  -> e
                  -> SimulationParams e
                  -> (ReactHandle () (SimulationStepOut o)
                          -> Bool
                          -> SimulationStepOut o
                          -> IO Bool)
                  -> IO (ReactHandle () (SimulationStepOut o))
simulateIOInit adefs e params iterFunc = reactInit (return ()) iterFunc (simulate params adefs e)
-------------------------------------------------------------------------------
-}

  {-
simulateTimeDeltas :: [AgentDef m o d]
                   -> e
                   -> SimulationParams
                   -> [DTime]
                   -> [SimulationStepOut o]
simulateTimeDeltas adefs e params dts = 
  where
    sts = zip dts (repeat Nothing) 
    ticks = repeat ()
    embed (simulate params adefs e) ((), sts)

simulateAggregateTimeDeltas :: [AgentDef m o d]
                            -> e
                            -> SimulationParams
                            -> [DTime]
                            -> AgentObservableAggregator s e a
                            -> [a]
simulateAggregateTimeDeltas adefs e params dts aggrFun = seq agrs agrs -- optimization
  where
    sts = zip dts (repeat Nothing) 
    agrSf = arr aggrFun
    sf = simulate params adefs e >>> agrSf
    agrs = embed sf ((), sts)


simulateAggregateTime :: Monad m
                      => [AgentDef m o d e]
                      -> DTime
                      -> Time
                      -> AgentObservableAggregator o a -- SimulationStepOut o -> a
                      -> (m [[a]] -> [a])
                      -> [a]
simulateAggregateTime adefs dt t runM aggrFun = seq aoss aoss -- optimization
  where
    steps = floor $ t / dt
    ticks = replicate steps ()
    agrSf = arr aggrFun
    sf    = simulate adefs >>> agrSf

    aossReader = embed sf ticks
    aossState  = runReaderT aossReader dt
    aossM      = runStateT aossState absState
    (aoss, absStateFinal) = runM aossM

-}
----------------------------------------------------------------------------------------------------------------------

{-
------------------------------------------------------------------------------------------------------------------------
-- DEBUGGING THE SIMULATION USING HASKELL-TITAN
------------------------------------------------------------------------------------------------------------------------
simulateDebug :: forall s e m .
                (Show s, Read s, Show e, Read e)
                => [AgentDef m o d]
                -> e
                -> SimulationParams e
                -> Double
                -> (Bool -> SimulationStepOut o -> IO Bool)
                -> IO ()
simulateDebug adefs e params dt renderFunc = 
  simulateDebugInternal
    adefs
    e
    params
    (\_ -> return (dt, Nothing))
    renderFunc

simulateDebugInternal :: forall s e m .
                        (Show s, Read s, Show e, Read e)
                        => [AgentDef m o d]
                        -> e
                        -> SimulationParams e
                        -> (Bool -> IO (DTime, Maybe ()))
                        -> (Bool -> SimulationStepOut o -> IO Bool)
                        -> IO ()
simulateDebugInternal adefs e params inputFunc renderFunc = do
  bridge <- mkTitanCommTCPBridge

  reactimateControl
      bridge                                    -- Communication channels
      defaultPreferences                        -- Simulation preferences
      ([Pause] :: [Command (FooPred s e)])      --[Pause] -- ([] :: [Command FooPred])     -- Initial command queue
      (return ())                               -- IO a: Initial sensing action
      inputFunc                                 -- (Bool -> IO (DTime, Maybe a)): Continued sensing action
      renderFunc                                -- (Bool -> b -> IO Bool): Rendering/consumption action
      (simulate params adefs e)                 -- SF a b: Signal Function that defines the program 

data FooPred s e = FooPred deriving (Read, Show)

instance Pred (FooPred s e) () (SimulationStepOut o) where
    evalPred _ _ _ _ = True
    -}
----------------------------------------------------------------------------------------------------------------------