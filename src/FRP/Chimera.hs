module FRP.Chimera 
  (
  -- FRP.Chimera.Agent.Interface
    AgentId
  , AgentData
  , DataFilter

  , Agent
  , AgentRandom
  , AgentMonad
  , AgentRandomMonad
  , AgentTX

  , AgentDef (..)
  , AgentIn     -- only type constructor visible
  , AgentOut    -- only type constructor visible

  , agentId
  , createAgent
  , kill
  , isDead
  , agentOut
  , agentOutObservable
  , nextAgentId

  , onStart
  , onEvent

  , dataFlow
  , dataFlowTo
  , dataFlows
  , broadcastDataFlow
  , hasDataFlow
  , onDataFlow
  , onFilterDataFlow
  , onDataFlowFrom
  , onDataFlowType

  , isRequestTx
  , requestTxData
  , requestTxIn
  , requestTx
  , acceptTX
  , agentTXOut
  , txDataOut
  , commitTx
  , commitTxWithCont
  , abortTx
  , txDataIn
  , hasTxDataIn
  , isCommitTX
  , isAbortTX

  , agentObservable
  , updateAgentObservable
  , setAgentObservable

  , recInitAllowed
  , allowsRecOthers
  , recursive
  , unrecursive
  , isRecursive
  , agentRecursions

  -- FRP.Chimera.Agent.Monad
  , AgentMonadic

  , createAgentM
  , killM
  , isDeadM

  , dataFlowM
  , dataFlowToM
  , dataFlowsM
  , broadcastDataFlowM
  , onDataFlowMState
  , onDataFlowM

  , bypassEnvironment

  , updateAgentObservableM
  , agentObservableM
  , setAgentObservableM
  , agentObservableFieldM

  , agentMonadic

  , ifThenElse
  , ifThenElseM

  -- FRP.Chimera.Agent.Reactive
  -- NOTHING FOR NOW

  -- FRP.Chimera.Agent.Stream
  , dataFlowS
  , dataFlowsS
  
  , agentObservableS
  , setAgentObservableS
  
  -- FRP.Chimera.Environment.Continuous
  , Continuous2dDimension
  , Continuous2dCoord

  , Continuous2d -- only export type-constructor 

  , Continuous2dEmpty
  , DistanceFunction

  , createContinuous2d

  , stepTo
  , stepRandom

  , distanceManhattanCont2d
  , distanceEuclideanCont2d

  , wrapCont2d
  , wrapCont2dEnv

  , multCoord
  , addCoord
  , subCoord
  , vecFromCoords
  , vecLen
  , vecNorm
  , dotCoords

  , objectPresent
  , objectCoord
  , removeObject
  , updateObject
  , objectsInDistance

  -- FRP.Chimera.Environment.Definitions
  , Environment
  , EnvironmentMonadic

  , EnvironmentFolding

  , environmentMonadic

  -- FRP.Chimera.Environment.Discrete
  , Discrete2dDimension
  , Discrete2dCoord
  , Discrete2dNeighbourhood
  , Discrete2dCell

  , Discrete2d -- only export type-constructor 

  , SingleOccupantCell
  , SingleOccupantDiscrete2d
  , MultiOccupantCell
  , MultiOccupantDiscrete2d

  , createDiscrete2d

  , dimensionsDisc2d
  , dimensionsDisc2dM

  , allCellsWithCoords
  , updateCells
  , updateCellsM
  , updateCellsWithCoords
  , updateCellsWithCoordsM
  , updateCellAt
  , changeCellAt
  , changeCellAtM
  , cellsAroundRadius
  , cellsAroundRadiusM
  , cellsAroundRect
  , cellsAt
  , cellAt
  , cellAtM
  , randomCell
  , randomCellWithinRect
  --, environmentDisc2dRandom

  , neighbours
  , neighboursM
  , neighbourCells
  , neighbourCellsM

  , neighboursInNeumannDistance
  , neighboursInNeumannDistanceM
  , neighboursCellsInNeumannDistance
  , neighboursCellsInNeumannDistanceM

  , distanceManhattanDisc2d
  , distanceEuclideanDisc2d
  , neighbourhoodOf
  , neighbourhoodScale
  , wrapCells
  , neumann
  , moore
  , wrapNeighbourhood
  , wrapDisc2d
  , wrapDisc2dEnv

  , randomNeighbourCell
  , randomNeighbour
  
  , occupied
  , unoccupy
  , occupy
  , occupier
  , addOccupant
  , removeOccupant
  , hasOccupiers
  , occupiers

  -- FRP.Chimera.Environment.Network
  , NetworkType (..)
  , DeterministicNetwork (..)
  , RandomNetwork (..)
  , Network (..)

  , createNetwork
  , createDeterministicNetwork
  , createRandomNetwork
  , createEmptyNetwork
  , createNetworkWithGraph

  , constEdgeLabeler
  , unitEdgeLabeler

  , nodesOfNetwork
  , networkDegrees
  , neighbourNodes
  , neighbourEdges
  , neighbourAgentIds
  , neighbourAgentIdsM
  , neighbourLinks
  , directLinkBetween
  , directLinkBetweenM

  , randomNeighbourNode

  -- FRP.Chimera.Environment.Spatial
  , EnvironmentWrapping (..)

  -- FRP.Chimera.Environment.Utils
  , cont2dToDisc2d
  , disc2dToCont2d

  , cont2dTransDisc2d
  , disc2dTransCont2d

  -- FRP.Chimera.Random.Monadic
  , randomBoolM
  , randomExpM
  , randomElemM
  , avoidM

  -- FRP.Chimera.Random.Pure
  , fisherYatesShuffle

  -- FRP.Chimera.Random.Stream
  , randomBoolS
  , randomBoolS_

  , randomElemS
  , randomElemS_

  , randomExpS
  , randomExpS_
  
  , randomRandS
  , randomRandS_
  
  -- FRP.Chimera.Reactive.Extensions
  , superSamplingUniform
  , afterExp

  -- FRP.Chimera.Reactive.Transitions
  , EventSource
  
  , transitionAfter
  , transitionAfterExpSS
  , transitionAfterExp

  , transitionWithUniProb
  , transitionWithExpProb

  , transitionOnEvent

  , transitionOnObservablePred
  , transitionOnData
  -- , transitionOnEventWithGuard

  -- FRP.Chimera.Reactive.DataFlow
  , DataSource
  , DataFlowSF

  , dataFlowOccasionally
  , dataFlowOccasionallySrc
  
  , dataFlowOccasionallySS
  , dataFlowOccasionallySrcSS

  , constDataReceiverSource
  , constDataSource
  , randomNeighbourNodeMsgSource
  , randomNeighbourCellMsgSource
  , randomAgentIdMsgSource

  -- Problem: reacthandle is missing
  -- FRP.Chimera.Rendering.Continuous2d
  {-
  , AgentRendererCont2d
  , AgentColorerCont2d
  , AgentCoordCont2d
  , EnvRendererCont2d

  , renderFrameCont2d

  , defaultEnvRendererCont2d
  , voidEnvRendererCont2d

  , defaultAgentRendererCont2d
  , defaultAgentColorerCont2d
  , voidAgentRendererCont2d

  -- FRP.Chimera.Rendering.Discrete2d
  , AgentRendererDisc2d
  , AgentCellColorerDisc2d
  , AgentCoordDisc2d
  , EnvRendererDisc2d
  , EnvCellColorerDisc2d

  , renderFrameDisc2d

  , defaultEnvRendererDisc2d
  , defaultEnvColorerDisc2d
  , voidEnvRendererDisc2d

  , defaultAgentRendererDisc2d
  , defaultAgentColorerDisc2d
  , voidAgentRendererDisc2d

  , transformToWindow

  -- FRP.Chimera.Rendering.GlossSimulator
  , StepCallback
  , RenderFrame

  , simulateAndRender
  , simulateStepsAndRender
  
  --, debugAndRender

  -- FRP.Chimera.Rendering.Network
  , AgentRendererNetwork
  , AgentColorerNetwork

  , renderFrameNetwork

  , defaultAgentRendererNetwork
  , defaultAgentColorerNetwork
-}

  -- FRP.Chimera.SD.Definitions
  , StockId
  , FlowId

  , Stock
  , Flow
  , SDObservable
  , SDDef

  , runSD

  , createStock
  , createFlow

  , flowInFrom
  , stockInFrom

  , flowOutTo
  , flowOutToM
  , flowOutToS

  , stockOutTo
  , stockOutToM
  , stockOutToS

  -- FRP.Chimera.Simulation.Init 
  , SimulationParams (..)

  , initRng
  , initSimulation
  , newAgentId

  -- FRP.Chimera.Simulation.Replication   
  , AgentDefReplicator
  , Replication
  
  , ReplicationConfig (..)

  , defaultAgentReplicator

  , runReplications
  , runReplicationsWithAggregation

  -- FRP.Chimera.Simulation.Simulation
  , AgentObservableAggregator
  
  --, simulateIOInit

  , simulateTime
  --, simulateTimeDeltas
  --, simulateAggregateTimeDeltas
  , simulateAggregateTime

  -- FRP.Chimera.Simulation.Common 
  , AgentObservable
  ) where

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Monad
import FRP.Chimera.Agent.Stream

import FRP.Chimera.Environment.Continuous
import FRP.Chimera.Environment.Definitions
import FRP.Chimera.Environment.Discrete
import FRP.Chimera.Environment.Network
import FRP.Chimera.Environment.Spatial
import FRP.Chimera.Environment.Utils

import FRP.Chimera.Random.Monadic
import FRP.Chimera.Random.Pure
import FRP.Chimera.Random.Stream

import FRP.Chimera.Reactive.DataFlow
import FRP.Chimera.Reactive.Extensions
import FRP.Chimera.Reactive.Transitions

-- Problem: reacthandle is missing
--import FRP.Chimera.Rendering.Continuous2d
--import FRP.Chimera.Rendering.Discrete2d
--import FRP.Chimera.Rendering.GlossSimulator
--import FRP.Chimera.Rendering.Network

import FRP.Chimera.SD.Definitions
import FRP.Chimera.Simulation.Init 
import FRP.Chimera.Simulation.Replication   
import FRP.Chimera.Simulation.Simulation
import FRP.Chimera.Simulation.Common

{-
------------------------------------------------------------------------------------------------------------------------
-- TODOs
------------------------------------------------------------------------------------------------------------------------
- we have a terrible memory leak at the moment...

- different build targets: gloss rendering
  - add HOW-TO of Chimera examples running on github in a sandbox
  
  - clean-up, can all be done in one rush through ALL the files:
    -> STYLE, INDENTATIONS & COMMENTS:		https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
    -> WARNINGS:							no warnings with -Wall -Werror

- allow to be able to stop simulation when iteration-function returns True

- adding a scheduling language in AgentDef?
  , e.g. allows to schedule in case of specific events like the watchee construct of repast. 
  , benefit: statically typed. but how to implement? dont want too tight coupling between environments 
  , should be as generic as possible. functions as parameters could solve this. only in case if sequential 
  , update strategy. does this imply that an agent is then scheduled more often? yes. zombies would specify: 
  , randomorder with a given dt humans: event-driven

  , simparams: introduce different scheduling orderings: e.g. ascending/descebdibg by e.g. agentid or 
  ,   , random or in order or reverse order

  ,  but arent we then working towards a DES? does it still allow SD and continuous ABS with time-semantics? 
  ,   , if yes then we have a combination of all

- implement Graph-Renderer
- run all rendering-stuff in IO?
- AgentRenderer: Circle/Rectangle as shapes
- GlossSimulator: pass Background-color as additional parameters (use currying)

- clean-up
  , - imports: no unused imports
  , - lint: must be clear of warnings
  , - warnings: compilation with -w must show no warnings at all
  , 
- comment haskell code
------------------------------------------------------------------------------------------------------------------------
-}