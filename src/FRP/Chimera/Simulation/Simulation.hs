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
             => [AgentDef m o d]
             -> SimulationParams
             -> DTime
             -> Time
             -> m [SimulationStepOut o]
simulateTime adefs params dt t = agrs
  where
    steps = floor $ t / dt
    ticks = replicate steps ()
    aossM = embed (simulate params adefs) ticks
    agrs = runReaderT aossM dt

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
-}

simulateAggregateTime :: Monad m
                      => [AgentDef m o d]
                      -> SimulationParams
                      -> DTime
                      -> Time
                      -> AgentObservableAggregator o a
                      -> m [a]
simulateAggregateTime adefs params dt t aggrFun = seq agrs agrs -- optimization
  where
    steps = floor $ t / dt
    ticks = replicate steps ()
    agrSf = arr aggrFun
    sf = simulate params adefs >>> agrSf

    aossM = embed sf ticks
    agrs = runReaderT aossM dt


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

----------------------------------------------------------------------------------------------------------------------
simulate :: Monad m
         => SimulationParams
         -> [AgentDef m o d]
         -> SF m () (SimulationStepOut o)
simulate params adefs = simulatePar params asfs ais
  where
    asfs = map adBeh adefs
    idGen = simIdGen params
    ais = startingAgentIn adefs idGen
----------------------------------------------------------------------------------------------------------------------