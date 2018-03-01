module FRP.Chimera.Simulation.Simulation 
  (
    AgentObservableAggregator

  --, simulateIOInit

  , simulate
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
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQ
import           FRP.BearRiver

import           FRP.Chimera.Agent.Interface
import           FRP.Chimera.Agent.Monad 
import           FRP.Chimera.Simulation.Common
import           FRP.Chimera.Simulation.ParIteration

type AgentObservableAggregator o a  = SimulationStepOut o -> a

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

-------------------------------------------------------------------------------
-- RUN THE SIMULATION FOR A FIXED TIME
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

-- TODO: output the monadic context every dt together with the agent outs
simulate :: Monad m
         => [AgentDef m o d e]
         -> DTime
         -> Time
         -> m a
         -> m (Time, Integer, [(Time, a)])
simulate adefs tSampling tLimit samplingFunc = do
    ((asfs, ais), abs0) <- runStateT (startingAgentM adefs) absState
    
    let asMap = Prelude.foldr (\(ai, asf) acc -> Map.insert (aiId ai) asf acc) Map.empty (Prelude.zip ais asfs)
    (samples, absFinal) <- runStateT (stepClock 0 asMap samplingFunc []) abs0
    
    let finalTime   = absTime absFinal
    let finalEvtCnt = absEvtIdx absFinal

    return (finalTime, finalEvtCnt, reverse samples)

  where
    stepClock :: Monad m 
              => Double
              -> Map.Map AgentId (AgentCont m o d e)
              -> m a
              -> [(Time, a)] 
              -> (ABSMonad m e) [(Time, a)]
    stepClock ts asMap samplingFunc acc = do
      q <- gets absEvtQueue

      -- TODO: use MaybeT 

      let mayHead = PQ.getMin q
      if isNothing mayHead
        then return acc
        else do
          ec <- gets absEvtIdx
          t  <- gets absTime

          let _qi@(QueueItem aid e t') = fromJust mayHead
          --let q' = Debug.Trace.trace ("QueueItem: " ++ show qi) (PQ.drop 1 q)
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
          let localDt = tSampling

          let acReader = unMSF ac ai
          (_ao, ac') <- runReaderT acReader localDt
         
          let asMap' = Map.insert aid ac' asMap

          s <- lift samplingFunc
          let (acc', ts') = if ts >= tSampling
                then ((t', s) : acc, 0)
                else (acc, ts + (t' - t))

          if t' < tLimit
            then stepClock ts' asMap' samplingFunc acc'
            else return acc'

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