{-# LANGUAGE Arrows             #-}
{-# LANGUAGE FlexibleContexts   #-}
module Agent 
  (
    sirAgentBehaviour
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.Chimera
import FRP.Yampa

import Model

-------------------------------------------------------------------------------
-- Non-Reactive Functions
-------------------------------------------------------------------------------
gotInfected :: RandomGen g => FrSIRAgentIn -> Rand g Bool
gotInfected ain = onDataFlowM gotInfectedAux ain False
  where
    gotInfectedAux :: RandomGen g => Bool -> AgentData FrSIRData -> Rand g Bool
    gotInfectedAux False (_, Contact Infected) = randomBoolM infectivity
    gotInfectedAux x _ = return x

respondToContactWith :: RandomGen g
                     => SIRState 
                     -> FrSIRAgentIn 
                     -> StateT (FrSIRAgentOut g) (FrSIRAgentMonad g) ()
respondToContactWith state ain = 
    onDataFlowMState respondToContactWithAux ain
  where
    respondToContactWithAux :: RandomGen g
                            => AgentData FrSIRData 
                            -> StateT (FrSIRAgentOut g) (FrSIRAgentMonad g) ()
    respondToContactWithAux (senderId, Contact _) = 
      dataFlowM (senderId, Contact state)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reactive Functions
-------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirAgentSuceptible :: RandomGen g => FrSIREnvironment -> FrSIRAgent g
sirAgentSuceptible env = 
  transitionOnEvent 
    sirAgentInfectedEvent 
    (sirAgentSusceptibleBehaviour env) 
    (const $ sirAgentInfected)

sirAgentInfectedEvent :: RandomGen g => FrSIREventSource g ()
sirAgentInfectedEvent = proc ain -> do
    isInfected <- arrM (\ain -> lift $ lift $ gotInfected ain) -< ain
    infectionEvent <- edge -< isInfected
    returnA -< infectionEvent

sirAgentSusceptibleBehaviour :: RandomGen g 
                             => FrSIREnvironment 
                             -> FrSIRAgent g
sirAgentSusceptibleBehaviour env = proc ain -> do
    setAgentObservableS -< Susceptible
    ds <- dataFlowOccasionallySrcSS 
            (1 / contactRate) 
            contactSS 
            (randomAgentIdMsgSource env (Contact Susceptible) True) -< ain
    dataFlowsS -< ds
    returnA -< ()

-- INFECTED
sirAgentInfected :: RandomGen g => FrSIRAgent g
sirAgentInfected = 
  transitionAfterExpSS 
    illnessDuration 
    illnessTimeoutSS 
    sirAgentInfectedBehaviour
    sirAgentRecovered

sirAgentInfectedBehaviour :: RandomGen g => FrSIRAgent g
sirAgentInfectedBehaviour = proc ain -> do
    setAgentObservableS -< Infected
    arrM (\ain -> lift $ respondToContactWith Infected ain) -< ain
    returnA -< ()

-- RECOVERED
sirAgentRecovered :: RandomGen g => FrSIRAgent g
sirAgentRecovered = proc _ -> do
  setAgentObservableS -< Recovered
  returnA -< ()

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g 
                  => SIRState 
                  -> FrSIREnvironment 
                  -> FrSIRAgent g
sirAgentBehaviour Susceptible env = sirAgentSuceptible env
sirAgentBehaviour Infected    _   = sirAgentInfected
sirAgentBehaviour Recovered   _   = sirAgentRecovered
-------------------------------------------------------------------------------