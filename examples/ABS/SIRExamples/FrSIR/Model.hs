module Model 
  (
    SIRState (..)
  , FrSIRData (..)

  , FrSIRAgentState
  , FrSIREnvironment

  , FrSIRAgentMonad
  
  , FrSIRAgent
  , FrSIRAgentCont
  , FrSIRAgentDef
  , FrSIRAgentIn
  , FrSIRAgentOut
  , FrSIRAgentObservable
  
  , FrSIREventSource
  , FrSIRReplicationConfig
  , FrSIRAgentDefReplicator
  
  , infectivity
  , contactRate
  , illnessDuration

  , contactSS
  , illnessTimeoutSS
  ) where

import Control.Monad.Random

import FRP.Chimera

-------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
-------------------------------------------------------------------------------
data SIRState         = Susceptible | Infected | Recovered deriving (Eq, Show)
data FrSIRData        = Contact SIRState deriving (Eq, Show)

type FrSIRAgentState  = SIRState

-- NOTE: here we are not interested in the network and their influences, instead we go for a fully-connected network
-- We could implement this using Network () as the environment type but the underlying graph-library (FGL) cannot
-- deal with big (>10.000 nodes) complete networks as it sucks up massive memory. 
type FrSIREnvironment = [AgentId]

type FrSIREvent = ()
type FrSIRAgentMonad g         = Rand g

type FrSIRAgent g              = Agent     (FrSIRAgentMonad g) FrSIRAgentState FrSIRData FrSIREvent
type FrSIRAgentCont g          = AgentCont (FrSIRAgentMonad g) FrSIRAgentState FrSIRData FrSIREvent
type FrSIRAgentDef g           = AgentDef  (FrSIRAgentMonad g) FrSIRAgentState FrSIRData FrSIREvent
type FrSIRAgentIn              = AgentIn                       FrSIRAgentState FrSIRData FrSIREvent
type FrSIRAgentOut g           = AgentOut  (FrSIRAgentMonad g) FrSIRAgentState FrSIRData FrSIREvent
type FrSIRAgentObservable      = AgentObservable               FrSIRAgentState

type FrSIREventSource g a      = EventSource        (FrSIRAgentMonad g) FrSIRAgentState FrSIRData FrSIREvent a
type FrSIRReplicationConfig g  = ReplicationConfig  (FrSIRAgentMonad g) FrSIRAgentState FrSIRData FrSIREvent
type FrSIRAgentDefReplicator g = AgentDefReplicator (FrSIRAgentMonad g) FrSIRAgentState FrSIRData FrSIREvent
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MODEL-PARAMETERS
-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

-- average number of contacts per time-unit
contactRate :: Double
contactRate = 5

-- average duration of illnes in time-units
illnessDuration :: Double
illnessDuration = 15

-- number of super-samples for contact-rate: because of high contact rate per time-unit we need an even higher number of samples
contactSS :: Int
contactSS = 20

-- number of super-samples for illness duration time-out: because the duration is quite long on average we can sample it with low frequency (low number of samples)
illnessTimeoutSS :: Int
illnessTimeoutSS = 2
-------------------------------------------------------------------------------