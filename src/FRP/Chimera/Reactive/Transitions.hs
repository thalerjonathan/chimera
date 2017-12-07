{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Reactive.Transitions
  (
    EventSource

  , transitionAfter
  , transitionAfterExpSS
  , transitionAfterExp

  , transitionWithUniProb
  , transitionWithExpProb

  , transitionOnEvent

  , transitionOnObservablePred
  , transitionOnData
  -- , transitionOnEventWithGuard
  ) where

-- import Data.Maybe

import Control.Monad.Random
--import Control.Monad.State
import Control.Monad.State.Strict
import FRP.BearRiver

--import FRP.Chimera.Agent.Monad
import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Stream
--import FRP.Chimera.Random.Monadic 
import FRP.Chimera.Random.Stream
import FRP.Chimera.Reactive.Extensions 

type EventCont m o d e a          = a -> Agent m o d e
type EventSource m o d e a        = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) (Event a)
-- only for internal use
type TransitionEventSF m o d e a  = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) (e, Event a)

-- TODO: refactor some functionality into a general combinator e.g. all probability transitions


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
                       -> TransitionEventSF m o d e ()
    transitionAfterAux t from = proc aie -> do
      aoe <- from       -< aie
      evt <- after t () -< ()
      returnA -< (aoe, evt)

transitionAfterExpSS :: MonadRandom m 
                     => Double
                     -> Int
                     -> Agent m o d e
                     -> Agent m o d e
                     -> Agent m o d e
transitionAfterExpSS t ss from to = switch (transitionAfterExpSSAux from) (const to)
  where
    transitionAfterExpSSAux :: MonadRandom m 
                            => Agent m o d e 
                            -> TransitionEventSF m o d e ()
    transitionAfterExpSSAux from = proc aie -> do
      e     <- from                                     -< aie
      evts  <- superSamplingUniform ss (afterExp t ())  -< ()

      let hasEvent  = any isEvent evts
          evt       = if hasEvent 
                        then Event () 
                        else NoEvent

      returnA -< (e, evt)

transitionAfterExp :: MonadRandom m 
                   => Double
                   -> Agent m o d e
                   -> Agent m o d e
                   -> Agent m o d e
transitionAfterExp t from to = switch (transitionAfterExpAux from) (const to)
  where
    transitionAfterExpAux :: MonadRandom m
                          => Agent m o d e 
                          -> TransitionEventSF m o d e ()
    transitionAfterExpAux from = proc aie -> do
        e       <- from           -< aie
        evt     <- afterExp t ()  -< ()
        returnA -< (e, evt)

transitionWithUniProb :: MonadRandom m 
                      => Double
                      -> Agent m o d e
                      -> Agent m o d e
                      -> Agent m o d e
transitionWithUniProb p from to = switch (transitionWithUniProbAux from) (const to)
  where
    transitionWithUniProbAux :: MonadRandom m 
                             => Agent m o d e
                             -> TransitionEventSF m o d e ()
    transitionWithUniProbAux from = proc aie -> do
      e       <- from           -< aie
      evtFlag <- randomBoolS p  -< ()
      evt     <- edgeFrom False -< evtFlag
      returnA -< (e, evt)

transitionWithExpProb :: MonadRandom m
                      => Double
                      -> Double
                      -> Agent m o d e
                      -> Agent m o d e
                      -> Agent m o d e
transitionWithExpProb lambda p from to = switch (transitionWithExpProbAux from) (const to)
  where
    transitionWithExpProbAux :: MonadRandom m  
                             => Agent m o d e
                             -> TransitionEventSF m o d e ()
    transitionWithExpProbAux from = proc aie -> do
      e       <- from               -< aie
      r       <- randomExpS lambda  -< ()
      evt     <- edgeFrom False     -< (p >= r)
      returnA -< (e, evt)


transitionOnEvent :: Monad m
                  => EventSource m o d e a
                  -> Agent m o d e
                  -> EventCont m o d e a
                  -> Agent m o d e
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) to
  where
    transitionEventAux :: Monad m
                       => EventSource m o d e a
                       -> Agent m o d e
                       -> TransitionEventSF m o d e a
    transitionEventAux evtSrc from = proc aie@(ain, _) -> do
      e       <- from     -< aie
      evt     <- evtSrc   -< (ain, e)
      returnA -< (e, evt)

type ObservablePredicate o = o -> Bool

-- NOTE: assumes state isJust
transitionOnObservablePred :: Monad m
                           => ObservablePredicate o
                           -> Agent m o d e
                           -> Agent m o d e
                           -> Agent m o d e
transitionOnObservablePred pred from to = 
    switch (transitionOnObservablePredAux pred from) (const to)
  where
    transitionOnObservablePredAux :: Monad m
                                  => ObservablePredicate o
                                  -> Agent m o d e
                                  -> TransitionEventSF m o d e ()
    transitionOnObservablePredAux pred from = proc aie -> do
      e       <- from             -< aie
      obs     <- agentObservableS -< ()
      evt     <- edgeFrom False   -< pred obs
      returnA -< (e, evt)

transitionOnData :: (Monad m, Eq d)
                 => d 
                 -> Agent m o d e
                 -> EventCont m o d e ()
                 -> Agent m o d e
transitionOnData d from to = transitionOnEvent (dataEventSource d) from to

-- NOTE: for now this is not implemented as it can be implemented in an
-- event-source with an agent running in a monad-stack containing a MonadRandom
{-
transitionOnEventWithGuard :: (MonadRandom m, RandomGen g)
                           => EventSource m o d e
                           -> Rand g Bool
                           -> Agent m o d e
                           ->  EventCont m o d e a
                           -> Agent m o d e
transitionOnEventWithGuard evtSrc guardAction from to = 
    switch (transitionEventWithGuardAux evtSrc from) (const to)
  where
    transitionEventWithGuardAux :: MonadRandom m
                                => EventSource m o d e a
                                -> Agent m o d e 
                                -> TransitionEventSF m o d e a
    transitionEventWithGuardAux evtSrc from = proc aie@(ain, _) -> do
      e     <- from                       -< aie
      evt   <- evtSrc                     -< (ain, e)
      flag  <- arrM_ (lift $ guardAction) -< ()
      returnA -< if (isEvent evt && flag)
                  then (e, Event())
                  else (e, NoEvent)
-}

dataEventSource :: (Eq d, Monad m) => d -> EventSource m o d e ()
dataEventSource d = proc (ain, _) -> do
  evt <- edgeFrom False -< hasDataFlow d ain
  returnA -< evt
