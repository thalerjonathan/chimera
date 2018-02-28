module Init 
  (
    createSIRNumInfected
  ) where

import Control.Monad.Random
import FRP.Chimera

import Agent
import Model

createSIRNumInfected :: RandomGen g
                     => Int 
                     -> Int 
                     -> Rand g ([SIRAgentDef g], SIREnvironment)
createSIRNumInfected agentCount numInfected = do
  let agentIds = [0 .. (agentCount-1)]
      infIds   = take numInfected agentIds
      susIds   = drop numInfected agentIds

  adefsSus <- mapM (sirAgent Susceptible agentIds) susIds
  adefsInf <- mapM (sirAgent Infected agentIds) infIds

  return (adefsSus ++ adefsInf, agentIds)

sirAgent :: RandomGen g
         => SIRState
         -> SIREnvironment
         -> AgentId
         -> Rand g (SIRAgentDef g)
sirAgent s env aid = 
  return AgentDef { 
    adId        = aid
  , adBeh       = sirAgentBehaviour s env
  , adInitData  = []
  }