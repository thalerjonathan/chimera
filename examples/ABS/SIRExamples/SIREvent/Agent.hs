{-# LANGUAGE Arrows #-}
module Agent 
  (
    sirAgentBehaviour
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver
import FRP.Chimera

import Model

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
modifySIRStateM :: RandomGen g
                => (SIRAggregateState -> SIRAggregateState) 
                -> (SIRAgentMonadT g) ()
modifySIRStateM = lift . modify

incSus :: SIRAggregateState -> SIRAggregateState
incSus (s, i, r) = (s+1, i, r)

decSus :: SIRAggregateState -> SIRAggregateState
decSus (s, i, r) = (s-1, i, r)

incInf :: SIRAggregateState -> SIRAggregateState
incInf (s, i, r) = (s, i+1, r)

decInf :: SIRAggregateState -> SIRAggregateState
decInf (s, i, r) = (s, i-1, r)

incRec :: SIRAggregateState -> SIRAggregateState
incRec (s, i, r) = (s, i, r+1)

drawRandomIllnessDuration :: RandomGen g 
                          => (SIRAgentMonadT g) Double
drawRandomIllnessDuration = lift $ lift (randomExpM (1 / illnessDuration))

-------------------------------------------------------------------------------
-- Reactive Functions
-------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirAgentSusceptible :: RandomGen g 
                    => SIREnvironment 
                    -> SIRAgent g
sirAgentSusceptible env aid = do
    modifySIRStateM incSus
    _ <- scheduleEventM aid MakeContact contactInterval
    return $ sirAgentSusceptibleCont env aid

sirAgentSusceptibleCont :: RandomGen g
                        => SIREnvironment 
                        -> AgentId 
                        -> SIRAgentCont g
sirAgentSusceptibleCont env aid = 
    switch
      susceptibleAgentInfected
      (const $ sirAgentInfectedCont aid)
  where
    susceptibleAgentInfected :: RandomGen g
                             => SF
                                (SIRAgentMonadT g)
                                SIRAgentIn
                                (SIRAgentOut g, Event ()) 
    susceptibleAgentInfected = proc ain -> 
      if hasEvent ain
        then 
          case extractEvent ain of
            (Contact _ s) ->
              case s of
                Infected -> do
                  arrM_ (lift $ lift (modify decSus)) -< ()
                  arrM_ (lift $ lift (modify incInf)) -< ()
                  dt <- arrM_ (lift (randomExpM (1 / illnessDuration))) -< ()
                  _ <- arrM (lift . scheduleEventM aid Recover) -< dt
                  returnA -< (agentOut, Event ())
                _        -> returnA -< (agentOut, NoEvent)

            MakeContact -> do 
              receivers <- arrM_ (lift (forM [1..contactRate] (const $ randomElemM env))) -< ()
              arrM (lift . mapM_ (\r -> scheduleEventM
                r
                (Contact aid Susceptible)
                0.01)) -< receivers
              arrM_ (lift $ scheduleEventM aid MakeContact contactInterval) -< ()
              returnA -< (agentOut, NoEvent)

            -- Recover will never occur for a susceptible
            _ -> returnA -< (agentOut, NoEvent)

        else returnA -< (agentOut, NoEvent)

-- INFECTED
sirAgentInfected :: RandomGen g 
                 => SIRAgent g
sirAgentInfected aid = do
    modifySIRStateM incInf
    dt <- drawRandomIllnessDuration
    _  <- scheduleEventM aid Recover dt
    return $ sirAgentInfectedCont aid

sirAgentInfectedCont :: RandomGen g 
                     => AgentId 
                     -> SIRAgentCont g
sirAgentInfectedCont aid = 
    switch 
      sirInfectedAgentRecovered 
      (const sirAgentRecoveredCont)
  where
    sirInfectedAgentRecovered :: RandomGen g 
                              => SF
                                (SIRAgentMonadT g)
                                SIRAgentIn
                                (SIRAgentOut g, Event ()) 
    sirInfectedAgentRecovered = proc ain -> 
      if hasEvent ain
        then
          case extractEvent ain of
            (Contact sender s) ->
              case s of
                Susceptible -> do
                  _ <- arrM (\sender -> lift (scheduleEventM sender (Contact aid Infected) 0.01)) -< sender
                  returnA -< (agentOut, NoEvent)
                _ -> returnA -< (agentOut, NoEvent)

            Recover -> do
              arrM_ (lift $ lift (modify decInf)) -< ()
              arrM_ (lift $ lift (modify incRec)) -< ()
              returnA -< (agentOut, Event ())

            -- MakeContact will never occur for an infected agent
            _ -> returnA -< (agentOut, NoEvent)

        else returnA -< (agentOut, NoEvent)

-- RECOVERED
sirAgentRecovered :: RandomGen g 
                  => SIRAgent g
sirAgentRecovered _ = 
    modifySIRStateM incRec >> return sirAgentRecoveredCont

sirAgentRecoveredCont :: SIRAgentCont g
sirAgentRecoveredCont = arr (const agentOut)

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g 
                  => SIRState 
                  -> SIREnvironment 
                  -> SIRAgent g 
sirAgentBehaviour Susceptible env aid = sirAgentSusceptible env aid
sirAgentBehaviour Infected    _   aid = sirAgentInfected aid
sirAgentBehaviour Recovered   _   aid = sirAgentRecovered aid
-------------------------------------------------------------------------------