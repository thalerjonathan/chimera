{-# LANGUAGE Arrows             #-}
{-# LANGUAGE FlexibleContexts   #-}
module Agent 
  (
    sirAgentBehaviour
  ) where

import Control.Monad.Random
import FRP.BearRiver
import FRP.Chimera

import Model

-------------------------------------------------------------------------------
-- Non-Reactive Functions
-------------------------------------------------------------------------------
gotInfected :: RandomGen g => FrSIRAgentIn -> Rand g Bool
gotInfected ain = onDataFlowM gotInfectedAux ain False
  where
    gotInfectedAux :: RandomGen g => Bool -> DataFlow FrSIRData -> Rand g Bool
    gotInfectedAux False (_, Contact Infected) = randomBoolM infectivity
    gotInfectedAux x _ = return x

respondToContactWith :: RandomGen g
                     => SIRState 
                     -> FrSIRAgentIn 
                     -> FrSIRAgentOut g
                     -> FrSIRAgentOut g
respondToContactWith state = onDataFlow respondToContactWithAux
  where
    respondToContactWithAux :: DataFlow FrSIRData 
                            -> FrSIRAgentOut g
                            -> FrSIRAgentOut g
    respondToContactWithAux (senderId, Contact _) = 
      dataFlow (senderId, Contact state)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reactive Functions
-------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirAgentSuceptible :: RandomGen g => FrSIREnvironment -> FrSIRAgentCont g
sirAgentSuceptible env = 
    transitionOnEvent
      sirAgentInfectedEvent 
      (sirAgentSusceptibleBehaviour env) 
      (const sirAgentInfected)

  where
    sirAgentInfectedEvent :: RandomGen g => FrSIREventSource g ()
    sirAgentInfectedEvent = proc ain -> do
      _isInfected     <- arrM (lift . lift . gotInfected) -< ain
      infectionEvent <- edge -< True -- isInfected
      returnA -< infectionEvent

    sirAgentSusceptibleBehaviour :: RandomGen g 
                                => FrSIREnvironment 
                                -> FrSIRAgentCont g
    sirAgentSusceptibleBehaviour env = proc ain -> do
        ds <- dataFlowOccasionallySrcSS 
                (1 / contactRate) 
                contactSS 
                (randomAgentIdMsgSource env (Contact Susceptible) True) -< ain

        returnA -< dataFlows ds (agentOutObservable Susceptible)

-- INFECTED
sirAgentInfected :: RandomGen g => FrSIRAgentCont g
sirAgentInfected = 
    transitionAfterExpSS 
      illnessDuration 
      illnessTimeoutSS 
      sirAgentInfectedBehaviour
      sirAgentRecovered
  where
    sirAgentInfectedBehaviour :: RandomGen g => FrSIRAgentCont g
    sirAgentInfectedBehaviour = 
        arr (\ain -> respondToContactWith Infected ain aoInf)
      where
        aoInf = agentOutObservable Infected

-- RECOVERED
sirAgentRecovered :: RandomGen g => FrSIRAgentCont g
sirAgentRecovered = arr (const $ agentOutObservable Recovered)

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g 
                  => SIRState 
                  -> FrSIREnvironment 
                  -> FrSIRAgent g
sirAgentBehaviour Susceptible env = return $ sirAgentSuceptible env
sirAgentBehaviour Infected    _   = return sirAgentInfected
sirAgentBehaviour Recovered   _   = return sirAgentRecovered
-------------------------------------------------------------------------------