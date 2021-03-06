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

  -- , transitionOnObservablePred
  , transitionOnData
  -- , transitionOnEventWithGuard
  ) where

import Control.Monad.Random
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Random.Stream
import FRP.Chimera.Reactive.Extensions 

type EventCont m o d e a   = a -> AgentCont m o d e
type EventSource m o d e a = SF (ABSMonad m e) (AgentIn o d e) (Event a)

-- NOTE: internal use only:
type TransitionSF m o d e a = SF (ABSMonad m e) (AgentIn o d e) (AgentOut m o d e, Event a)

transitionAfter :: Monad m 
                => Double
                -> AgentCont m o d e
                -> AgentCont m o d e
                -> AgentCont m o d e
transitionAfter t from to = switch (transitionAfterAux t from) (const to)
  where
    transitionAfterAux :: Monad m 
                       => Double 
                       -> AgentCont m o d e
                       -> TransitionSF m o d e ()
    transitionAfterAux t from = proc ain -> do
      out <- from       -< ain
      evt <- after t () -< ()
      returnA -< (out, evt)

transitionAfterExpSS :: MonadRandom m 
                     => Double
                     -> Int
                     -> AgentCont m o d e
                     -> AgentCont m o d e
                     -> AgentCont m o d e
transitionAfterExpSS t ss from to = switch (transitionAfterExpSSAux from) (const to)
  where
    transitionAfterExpSSAux :: MonadRandom m 
                            => AgentCont m o d e 
                            -> TransitionSF m o d e ()
    transitionAfterExpSSAux from = proc ain -> do
      out   <- from                                     -< ain
      evts  <- superSamplingUniform ss (afterExp t ())  -< ()

      let hasEvent = any isEvent evts
          evt      = if hasEvent 
                      then Event () 
                      else NoEvent

      returnA -< (out, evt)

transitionAfterExp :: MonadRandom m 
                   => Double
                   -> AgentCont m o d e
                   -> AgentCont m o d e
                   -> AgentCont m o d e
transitionAfterExp t from to = switch (transitionAfterExpAux from) (const to)
  where
    transitionAfterExpAux :: MonadRandom m
                          => AgentCont m o d e 
                          -> TransitionSF m o d e ()
    transitionAfterExpAux from = proc ain -> do
        out     <- from           -< ain
        evt     <- afterExp t ()  -< ()
        returnA -< (out, evt)

transitionWithUniProb :: MonadRandom m 
                      => Double
                      -> AgentCont m o d e
                      -> AgentCont m o d e
                      -> AgentCont m o d e
transitionWithUniProb p from to = switch (transitionWithUniProbAux from) (const to)
  where
    transitionWithUniProbAux :: MonadRandom m 
                             => AgentCont m o d e
                             -> TransitionSF m o d e ()
    transitionWithUniProbAux from = proc ain -> do
      out     <- from           -< ain
      evtFlag <- randomBoolS p  -< ()
      evt     <- edgeFrom False -< evtFlag
      returnA -< (out, evt)

transitionWithExpProb :: MonadRandom m
                      => Double
                      -> Double
                      -> AgentCont m o d e
                      -> AgentCont m o d e
                      -> AgentCont m o d e
transitionWithExpProb lambda p from to = switch (transitionWithExpProbAux from) (const to)
  where
    transitionWithExpProbAux :: MonadRandom m  
                             => AgentCont m o d e
                             -> TransitionSF m o d e ()
    transitionWithExpProbAux from = proc ain -> do
      out     <- from               -< ain
      r       <- randomExpS lambda  -< ()
      evt     <- edgeFrom False     -< (p >= r)
      returnA -< (out, evt)


transitionOnEvent :: Monad m
                  => EventSource m o d e a
                  -> AgentCont m o d e
                  -> EventCont m o d e a
                  -> AgentCont m o d e
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) to
  where
    transitionEventAux :: Monad m
                       => EventSource m o d e a
                       -> AgentCont m o d e
                       -> TransitionSF m o d e a
    transitionEventAux evtSrc from = proc ain -> do
      out     <- from     -< ain
      evt     <- evtSrc   -< ain
      returnA -< (out, evt)

{-
type ObservablePredicate o = o -> Bool

-- NOTE: assumes state isJust
transitionOnObservablePred :: Monad m
                           => ObservablePredicate o
                           -> AgentCont m o d e
                           -> AgentCont m o d e
                           -> AgentCont m o d e
transitionOnObservablePred pred from to = 
    switch (transitionOnObservablePredAux pred from) (const to)
  where
    transitionOnObservablePredAux :: Monad m
                                  => ObservablePredicate o
                                  -> AgentCont m o d e
                                  -> TransitionSF m o d ()
    transitionOnObservablePredAux pred from = proc ain -> do
      out     <- from             -< ain
      obs     <- agentObservableS -< ()
      evt     <- edgeFrom False   -< pred obs
      returnA -< (out, evt)
-}

transitionOnData :: (Monad m, Eq d)
                 => d 
                 -> AgentCont m o d e
                 -> EventCont m o d e ()
                 -> AgentCont m o d e
transitionOnData d from to = transitionOnEvent (dataEventSource d) from to

-- NOTE: for now this is not implemented as it can be implemented in an
-- event-source with an agent running in a monad-stack containing a MonadRandom
{-
transitionOnEventWithGuard :: (MonadRandom m, RandomGen g)
                           => EventSource m o d e
                           -> Rand g Bool
                           -> AgentCont m o d e
                           ->  EventCont m o d a
                           -> AgentCont m o d e
transitionOnEventWithGuard evtSrc guardAction from to = 
    switch (transitionEventWithGuardAux evtSrc from) (const to)
  where
    transitionEventWithGuardAux :: MonadRandom m
                                => EventSource m o d a
                                -> AgentCont m o d e 
                                -> TransitionEventSF m o d a
    transitionEventWithGuardAux evtSrc from = proc aie@(ain, _) -> do
      e     <- from                       -< aie
      evt   <- evtSrc                     -< (ain, e)
      flag  <- arrM_ (lift $ guardAction) -< ()
      returnA -< if (isEvent evt && flag)
                  then (e, Event())
                  else (e, NoEvent)
-}

dataEventSource :: (Eq d, Monad m) => d -> EventSource m o d e ()
dataEventSource d = proc ain -> do
  evt <- edgeFrom False -< hasDataFlow d ain
  returnA -< evt
