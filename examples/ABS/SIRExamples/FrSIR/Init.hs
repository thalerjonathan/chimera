module Init 
  (
    createFrSIRNumInfected
  , sirAgentDefReplicator
  ) where

import Control.Monad.Random
import FRP.Chimera

import Agent
import Model

createFrSIRNumInfected :: RandomGen g
                       => Int 
                       -> Int 
                       -> IO ([FrSIRAgentDef g], FrSIREnvironment)
createFrSIRNumInfected agentCount numInfected = do
  let agentIds = [0 .. (agentCount-1)]
  let infectedIds = take numInfected agentIds
  let susceptibleIds = drop numInfected agentIds

  adefsSusceptible <- mapM (frSIRAgent Susceptible agentIds) susceptibleIds
  adefsInfected <- mapM (frSIRAgent Infected agentIds) infectedIds

  return (adefsSusceptible ++ adefsInfected, agentIds)

frSIRAgent :: RandomGen g
           => SIRState
           -> FrSIREnvironment
           -> AgentId
           -> IO (FrSIRAgentDef g)
frSIRAgent s env aid = 
  return AgentDef { 
    adId        = aid
  , adBeh       = sirAgentBehaviour s env
  , adInitData  = []
  }

sirAgentDefReplicator :: RandomGen g
                      => FrSIREnvironment
                      -> Int 
                      -> FrSIRAgentDefReplicator g
sirAgentDefReplicator env numInfected g ad = (ad', g')
  where
    (g', _) = split g
    s = if (adId ad) < numInfected then Infected else Susceptible 
    -- NOTE: also need to overwrite behaviour with one with a different RNG!
    beh = sirAgentBehaviour s env
    ad' = ad { adBeh = beh }