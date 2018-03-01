module Main where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.Chimera
import FRP.Yampa

import Init
import Model
import Sir

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 1.0

t :: DTime
t = 100

agentCount :: Int
agentCount = 1000

numInfected :: Int
numInfected = 1

main :: IO () 
main = do
  let g0 = mkStdGen rngSeed
      ((adefs, _), g) = runRand (createSIRNumInfected agentCount numInfected) g0
  
      aossState = simulate adefs dt t samplingFunc
      aossRand  = runStateT aossState (0,0,0)
      ((finalTime, finalEvtCnt, samples), _) = evalRand aossRand g
    
      dynamics = map sampleToDynamic samples

  putStrLn $ "simulation terminated at t = " ++ show finalTime ++
            " after " ++ show finalEvtCnt ++ " events"  
  writeSirDynamicsFile "sirEvent.m" dt 0 dynamics

sampleToDynamic :: (Time, SIRAggregateState) 
                -> (Time, Double, Double, Double)
sampleToDynamic (t, (s, i, r)) = 
  (t, fromIntegral s, fromIntegral i, fromIntegral r)

samplingFunc :: (SIRAgentMonad g) SIRAggregateState
samplingFunc = get