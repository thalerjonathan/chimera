module Model 
  (
    SIRState (..)
  , SIREvent (..)
  , SIREnvironment

  , SIRAgentMonad
  , SIRAgentMonadT
  , SIRAggregateState

  , SIRAgent
  , SIRAgentCont
  , SIRAgentDef
  , SIRAgentIn
  , SIRAgentOut
  
  , infectivity
  , contactRate
  , illnessDuration
  , contactInterval
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver
import FRP.Chimera

-------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
-------------------------------------------------------------------------------
data SIRState = Susceptible | Infected | Recovered deriving (Eq, Show)

-- NOTE: here we are not interested in the network and their influences, instead we go for a fully-connected network
-- We could implement this using Network () as the environment type but the underlying graph-library (FGL) cannot
-- deal with big (>10.000 nodes) complete networks as it sucks up massive memory. 
type SIREnvironment = [AgentId]

type SIRData = ()
data SIREvent
  = MakeContact
  | Contact AgentId SIRState
  | Recover
  deriving Eq

type SIRAggregateState = (Int, Int, Int)

type SIRAgentMonad g  = StateT SIRAggregateState (Rand g)
type SIRAgentMonadT g = ABSMonad (SIRAgentMonad g) SIREvent

type SIRAgent g     = Agent     (SIRAgentMonad g) SIRState SIRData SIREvent
type SIRAgentCont g = AgentCont (SIRAgentMonad g) SIRState SIRData SIREvent
type SIRAgentDef g  = AgentDef  (SIRAgentMonad g) SIRState SIRData SIREvent
type SIRAgentIn     = AgentIn                     SIRState SIRData SIREvent
type SIRAgentOut g  = AgentOut  (SIRAgentMonad g) SIRState SIRData SIREvent
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MODEL-PARAMETERS
-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

-- average number of contacts per time-unit
contactRate :: Int
contactRate = 5

-- average duration of illnes in time-units
illnessDuration :: Double
illnessDuration = 15

contactInterval :: DTime
contactInterval = 1.0
-------------------------------------------------------------------------------