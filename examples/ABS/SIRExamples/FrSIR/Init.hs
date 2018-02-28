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
                       -> Rand g ([FrSIRAgentDef g], FrSIREnvironment)
createFrSIRNumInfected agentCount numInfected = do
  let agentIds = [0 .. (agentCount-1)]
      infIds   = take numInfected agentIds
      susIds   = drop numInfected agentIds

  adefsSus <- mapM (frSIRAgent Susceptible agentIds) susIds
  adefsInf <- mapM (frSIRAgent Infected agentIds) infIds

  return (adefsSus ++ adefsInf, agentIds)

frSIRAgent :: RandomGen g
           => SIRState
           -> FrSIREnvironment
           -> AgentId
           -> Rand g (FrSIRAgentDef g)
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
sirAgentDefReplicator env numInfected ad = ad'
  where
    s = if adId ad < numInfected then Infected else Susceptible 
    -- NOTE: also need to overwrite behaviour with one with a different RNG!
    beh = sirAgentBehaviour s env
    ad' = ad { adBeh = beh }