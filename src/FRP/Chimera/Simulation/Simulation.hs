module FRP.Chimera.Simulation.Simulation 
  (
    AgentObservableAggregator

  --, simulateIOInit

  , simulateTime
  --, simulateTimeDeltas
  --, simulateAggregateTimeDeltas
  , simulateAggregateTime

  -- , simulateDebug
  -- , simulateDebugInternal
  ) where

import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Simulation.Common
import FRP.Chimera.Simulation.Init
import FRP.Chimera.Simulation.ParIteration

type AgentObservableAggregator o e a  = SimulationStepOut o e-> a

{-
-------------------------------------------------------------------------------
-- RUNNING SIMULATION FROM AN OUTER LOOP
-------------------------------------------------------------------------------
simulateIOInit :: [AgentDef m o d e]
                  -> e
                  -> SimulationParams e
                  -> (ReactHandle () (SimulationStepOut s e)
                          -> Bool
                          -> SimulationStepOut s e
                          -> IO Bool)
                  -> IO (ReactHandle () (SimulationStepOut s e))
simulateIOInit adefs e params iterFunc = reactInit (return ()) iterFunc (simulate params adefs e)
-------------------------------------------------------------------------------
-}

-------------------------------------------------------------------------------
-- RUN THE SIMULATION FOR A FIXED TIME
-------------------------------------------------------------------------------

simulateTime :: Monad m
             => [AgentDef m o d e]
             -> e
             -> SimulationParams
             -> DTime
             -> Time
             -> m [SimulationStepOut o e]
simulateTime adefs e params dt t = agrs
  where
    steps = floor $ t / dt
    ticks = replicate steps ()
    aossM = embed (simulate params adefs e) ticks
    agrs = runReaderT aossM dt

  {-
simulateTimeDeltas :: [AgentDef m o d e]
                   -> e
                   -> SimulationParams
                   -> [DTime]
                   -> [SimulationStepOut s e]
simulateTimeDeltas adefs e params dts = 
  where
    sts = zip dts (repeat Nothing) 
    ticks = repeat ()
    embed (simulate params adefs e) ((), sts)

simulateAggregateTimeDeltas :: [AgentDef m o d e]
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
-}

simulateAggregateTime :: Monad m
                      => [AgentDef m o d e]
                      -> e
                      -> SimulationParams
                      -> DTime
                      -> Time
                      -> AgentObservableAggregator o e a
                      -> m [a]
simulateAggregateTime adefs e params dt t aggrFun = seq agrs agrs -- optimization
  where
    steps = floor $ t / dt
    ticks = replicate steps ()
    agrSf = arr aggrFun
    sf = simulate params adefs e >>> agrSf

    aossM = embed sf ticks
    agrs = runReaderT aossM dt


----------------------------------------------------------------------------------------------------------------------

{-
------------------------------------------------------------------------------------------------------------------------
-- DEBUGGING THE SIMULATION USING HASKELL-TITAN
------------------------------------------------------------------------------------------------------------------------
simulateDebug :: forall s e m .
                (Show s, Read s, Show e, Read e)
                => [AgentDef m o d e]
                -> e
                -> SimulationParams e
                -> Double
                -> (Bool -> SimulationStepOut s e -> IO Bool)
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
                        => [AgentDef m o d e]
                        -> e
                        -> SimulationParams e
                        -> (Bool -> IO (DTime, Maybe ()))
                        -> (Bool -> SimulationStepOut s e -> IO Bool)
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

instance Pred (FooPred s e) () (SimulationStepOut s e) where
    evalPred _ _ _ _ = True
    -}
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
simulate :: Monad m
         => SimulationParams
         -> [AgentDef m o d e]
         -> e
         -> SF m () (SimulationStepOut o e)
simulate params adefs e = simulatePar params asfs ais e 
  where
    asfs = map adBeh adefs
    idGen = simIdGen params
    ais = startingAgentIn adefs idGen
----------------------------------------------------------------------------------------------------------------------