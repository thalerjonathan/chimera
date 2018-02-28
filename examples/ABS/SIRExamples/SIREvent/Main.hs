module Main where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.Chimera
import FRP.Yampa

import Init

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 1.0

t :: DTime
t = 50

agentCount :: Int
agentCount = 100

numInfected :: Int
numInfected = 10

main :: IO () 
main = print s --writeSirDynamicsFile fileName dt 0 dynamics
  where
    g0 = mkStdGen rngSeed
    ((adefs, _), g) = runRand (createSIRNumInfected agentCount numInfected) g0
  
    aossState = simulateTime adefs dt t
    aossRand  = runStateT aossState (0,0,0)
    (_, s)    = evalRand aossRand g