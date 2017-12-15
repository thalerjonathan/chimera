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
sirAgentSuceptible :: RandomGen g => FrSIRAgent g
sirAgentSuceptible = 
  transitionOnEvent 
    sirAgentInfectedEvent 
    (readEnv $ sirAgentSusceptibleBehaviour) 
    (const $ sirAgentInfected)

sirAgentInfectedEvent :: RandomGen g => FrSIREventSource g ()
sirAgentInfectedEvent = proc (ain, e) -> do
    isInfected <- arrM (\ain -> lift $ lift $ gotInfected ain) -< ain
    infectionEvent <- edge -< isInfected
    returnA -< infectionEvent

sirAgentSusceptibleBehaviour :: RandomGen g => FrSIRAgentReadEnv g
sirAgentSusceptibleBehaviour = proc (ain, e) -> do
    setAgentObservableS -< Susceptible
    dataFlowOccasionallySrcSS 
            (1 / contactRate) 
            contactSS 
            (randomAgentIdMsgSource (Contact Susceptible) True) -< (ain, e)
    returnA -< ()

-- INFECTED
sirAgentInfected :: RandomGen g => FrSIRAgent g
sirAgentInfected = 
  transitionAfterExpSS 
    illnessDuration 
    illnessTimeoutSS 
    (ignoreEnv $ sirAgentInfectedBehaviour) 
    sirAgentRecovered

sirAgentInfectedBehaviour :: RandomGen g => FrSIRAgentIgnoreEnv g
sirAgentInfectedBehaviour = proc ain -> do
    setAgentObservableS -< Infected
    arrM (\ain -> lift $ respondToContactWith Infected ain) -< ain
    returnA -< ()

-- RECOVERED
sirAgentRecovered :: RandomGen g => FrSIRAgent g
sirAgentRecovered = proc (ain, e) -> do
  setAgentObservableS -< Recovered
  returnA -< e

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g => SIRState -> FrSIRAgent g
sirAgentBehaviour Susceptible = sirAgentSuceptible
sirAgentBehaviour Infected    = sirAgentInfected
sirAgentBehaviour Recovered   = sirAgentRecovered
-------------------------------------------------------------------------------