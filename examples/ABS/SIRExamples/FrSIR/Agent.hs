{-# LANGUAGE Arrows #-}
module Agent 
  (
    sirAgentBehaviour
  ) where

import Control.Monad.Random
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
                     -> FrSIRAgentOut g 
                     -> FrSIRAgentOut g
respondToContactWith state ain ao = 
    onDataFlow respondToContactWithAux ain ao
  where
    respondToContactWithAux :: RandomGen g
                            => AgentData FrSIRData 
                            -> FrSIRAgentOut g 
                            -> FrSIRAgentOut g
    respondToContactWithAux (senderId, Contact _) ao = 
      dataFlow (senderId, Contact state) ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reactive Functions
-------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirAgentSuceptible :: RandomGen g => g -> FrSIRAgent g
sirAgentSuceptible g = 
  transitionOnEvent 
    (sirAgentInfectedEvent g) 
    (readEnv $ sirAgentSusceptibleBehaviour g) 
    (sirAgentInfected g)

sirAgentInfectedEvent :: RandomGen g => g -> FrSIREventSource g ()
sirAgentInfectedEvent g = proc (ain, ao, e) -> do
    isInfected <- randomSF g -< gotInfected ain
    infectionEvent <- edge -< isInfected
    returnA -< infectionEvent

sirAgentSusceptibleBehaviour :: RandomGen g => g -> FrSIRAgentReadEnv g
sirAgentSusceptibleBehaviour g = proc (ain, e) -> do
    let ao = agentOutObs Susceptible
    ao' <- sendMessageOccasionallySrcSS 
            g 
            (1 / contactRate) 
            contactSS 
            (randomAgentIdMsgSource g (Contact Susceptible) True) -< (ain, ao, e)
    returnA -< ao'

-- INFECTED
sirAgentInfected :: RandomGen g => g -> FrSIRAgent g
sirAgentInfected g = 
  transitionAfterExpSS g illnessDuration illnessTimeoutSS (ignoreEnv $ sirAgentInfectedBehaviour g) sirAgentRecovered

sirAgentInfectedBehaviour :: RandomGen g => g -> FrSIRAgentIgnoreEnv g
sirAgentInfectedBehaviour g = proc ain -> do
    let ao = agentOutObs Infected
    returnA -< respondToContactWith Infected ain ao

-- RECOVERED
sirAgentRecovered :: RandomGen g => FrSIRAgent g
sirAgentRecovered = doNothingObs Recovered

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g => g -> SIRState -> FrSIRAgent g
sirAgentBehaviour g Susceptible = sirAgentSuceptible g
sirAgentBehaviour g Infected = sirAgentInfected g
sirAgentBehaviour _ Recovered = sirAgentRecovered
-------------------------------------------------------------------------------