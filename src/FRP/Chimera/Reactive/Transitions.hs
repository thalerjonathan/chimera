{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Reactive.Transitions
  (
    EventSource

  , transitionAfter
  , transitionAfterExpSS

    {-
  , transitionAfterExp
  , transitionWithUniProb
  , transitionWithExpProb
  , transitionOnEvent
  , transitionOnBoolState
  , transitionOnData
  , transitionOnEventWithGuard
  -}
  ) where

-- import Data.Maybe

import Control.Monad.Random
import Control.Monad.State
import FRP.BearRiver

--import FRP.Chimera.Agent.Monad
import FRP.Chimera.Agent.Interface
--import FRP.Chimera.Random.Monadic 
-- import FRP.Chimera.Random.Reactive

type EventSource m o d e = SF m (AgentIn o d e, e) (Event ())

-- TODO: refactor some functionality into a general combinator e.g. all probability transitions

-- only for internal use
type AgentEventSF m o d e = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) (e, Event ())

transitionAfter :: Monad m 
                => Double
                -> Agent m o d e
                -> Agent m o d e
                -> Agent m o d e
transitionAfter t from to = switch (transitionAfterAux t from) (const to)
  where
    transitionAfterAux :: Monad m 
                       => Double 
                       -> Agent m o d e 
                       -> AgentEventSF m o d e
    transitionAfterAux t from = proc aie -> do
      aoe <- from -< aie
      timeoutEvent <- after t () -< ()
      returnA -< (aoe, timeoutEvent)

transitionAfterExpSS :: Monad m 
                     => Double
                     -> Int
                     -> Agent m o d e
                     -> Agent m o d e
                     -> Agent m o d e
transitionAfterExpSS t ss from to = switch (transitionAfterExpSSAux t from) (const to)
  where
    transitionAfterExpSSAux :: Monad m 
                            => Double 
                            -> Agent m o d e 
                            -> AgentEventSF m o d e
    transitionAfterExpSSAux t from = proc aie -> do
      e <- from -< aie
      timeoutEvents <- superSamplingUniform ss (afterExp t ()) -< ()
      let hasEvent = any isEvent timeoutEvents
      let timeoutOccurred = if hasEvent then Event () else NoEvent
      returnA -< (aoe, timeoutOccurred)

afterExp :: MonadRandom m 
         => Time 
         -> SF m a (Event b)
afterExp tAvg sf = undefined -- switch () 

{-
transitionAfterExp :: RandomGen g => g 
                   -> Double
                   -> Agent m o d e
                   -> Agent m o d e
                   -> Agent m o d e
transitionAfterExp g t from to = switch (transitionAfterExpAux t from) (const to)
  where
  transitionAfterExpAux :: Double 
                        -> Agent m o d e 
                        -> SF m (AgentIn o d e, e) (e, Event ())
    transitionAfterExpAux t from = proc aie -> do
    aoe <- from -< aie
    timeoutEvent <- afterExp g t () -< ()
    returnA -< (aoe, timeoutEvent)

transitionWithUniProb :: RandomGen g => g 
                      -> Double
                      -> Agent m o d e
                      -> Agent m o d e
                      -> Agent m o d e
transitionWithUniProb g p from to = switch (transitionWithUniProbAux from)(const to)
  where
    transitionWithUniProbAux :: Agent m o d e
                             -> SF (AgentIn o d e, e) (e, Event ())
    transitionWithUniProbAux from = proc aie -> do
      aie' <- from -< aie
      evtFlag <- randomSF g -< randomBoolM p
      evt <- iEdge False -< evtFlag
      returnA -< (aie', evt)

transitionWithExpProb :: RandomGen g => g 
                      -> Double
                      -> Double
                      -> Agent m o d e
                      -> Agent m o d e
                      -> Agent m o d e
transitionWithExpProb g lambda p from to = switch (transitionWithExpProbAux from) (const to)
  where
    transitionWithExpProbAux :: Agent m o d e
                             -> SF (AgentIn o d e, e) (e, Event ())
    transitionWithExpProbAux from = proc aie -> do
      aie' <- from -< aie
      r <- randomSF g -< randomExpM lambda
      evt <- iEdge False -< (p >= r)
      returnA -< (aie', evt)

transitionOnEvent :: EventSource m o d e
                  -> Agent m o d e
                  -> Agent m o d e
                  -> Agent m o d e
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) (const to)
  where
    transitionEventAux :: EventSource m o d e
                       -> Agent m o d e
                       -> SF (AgentIn o d e, e) (e, Event ())
    transitionEventAux evtSrc from = proc aie@(ain, _) -> do
      (ao, e) <- from -< aie
      evt <- evtSrc -< (ain, ao, e)
      returnA -< ((ao, e), evt)

-- NOTE: assumes state isJust
transitionOnBoolState :: (s -> Bool)
                      -> Agent m o d e
                      -> Agent m o d e
                      -> Agent m o d e
transitionOnBoolState boolStateFunc from to = switch (transitionOnBoolStateAux boolStateFunc from) (const to)
  where
    transitionOnBoolStateAux :: (s -> Bool)
                             -> Agent m o d e
                             -> SF (AgentIn o d e, e) (e, Event ())
    transitionOnBoolStateAux boolStateFunc from = proc aie -> do
      (ao, e) <- from -< aie
      let state = fromJust $ aoState ao
      let evtFlag = boolStateFunc state
      evt <- iEdge False -< evtFlag
      returnA -< ((ao, e), evt)

transitionOnData :: Eq d => d 
                 -> Agent m o d e
                 -> Agent m o d e
                 -> Agent m o d e
transitionOnData d from to = transitionOnEvent (dataEventSource d) from to

transitionOnEventWithGuard :: (MonadRandom m, RandomGen g)
                           => EventSource m o d e
                           -> Rand g Bool
                           -> Agent m o d e
                           -> Agent m o d e
                           -> Agent m o d e
transitionOnEventWithGuard evtSrc guardAction from to = 
    switch (transitionEventWithGuardAux evtSrc from) (const to)
  where
    transitionEventWithGuardAux :: MonadRandom m
                => EventSource m o d e
                -> Agent m o d e 
                -> SF m (AgentIn o d e, e) (e, Event ())
    transitionEventWithGuardAux evtSrc from = proc aie@(ain, _) -> do
      e <- from -< aie
      evt <- evtSrc -< (ain, e)
      flag <- arrM_ (lift $ guardAction) -< ()
      returnA -< if (isEvent evt && flag)
                  then (e, Event())
                  else (e, NoEvent)

dataEventSource :: (Eq d, Monad m) => d -> EventSource m o d e
dataEventSource d = proc (ain, _) -> do
  evt <- edgeFrom False -< hasDataFlow d ain
  returnA -< evt
  -}