module Main (
    runSugarScapeWithRendering,
    runSugarScapeStepsAndRender,

    runSugarScapeStepsAndExport
  ) where

import FRP.FrABS

import Environment
import Exporter
import Init
import Renderer

winSize :: (Int, Int)
winSize = (800, 800)

winTitle :: String
winTitle = "SugarScape"

rngSeed :: Int
rngSeed = 42

agentCount :: Int
agentCount = 400

envSize :: (Int, Int)
envSize = (50, 50)


--updateStrat = Sequential    -- Sugarscape works ONLY with Sequential AND must be shuffled
--shuffleAgents = True        -- Sugarscape works ONLY with Sequential AND must be shuffled
--envCollapsing = Nothing
--envBeh = Just sugarScapeEnvironmentBehaviour

dt :: DTime
dt = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents

t :: Time
time = 200

frequency :: Int
frequency = 0

-- TODO: repair
-- BUG: all agents have id=0 because newAgentId seems not to hand out new ids...

main :: IO ()
main = runSugarScapeWithRendering

runSugarScapeWithRendering :: IO ()
runSugarScapeWithRendering = do
  params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- createSugarScape agentCount envSize params
  
  simulateAndRender initAdefs
                      initEnv
                      params
                      dt
                      frequency
                      winTitle
                      winSize
                      renderSugarScapeFrame
                      Nothing

runSugarScapeStepsAndRender :: IO ()
runSugarScapeStepsAndRender = do
  params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- createSugarScape agentCount envSize params
  
  simulateStepsAndRender initAdefs
                          initEnv
                          params
                          dt
                          time
                          winTitle
                          winSize
                          renderSugarScapeFrame

runSugarScapeStepsAndExport :: IO ()
runSugarScapeStepsAndExport = do
  params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- createSugarScape agentCount envSize params
  
  let asenv = simulateTime initAdefs initEnv params dt time
  writeSugarscapeDynamics asenv