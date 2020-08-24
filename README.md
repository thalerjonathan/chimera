# chimera
*NOTE: THIS PROJECT HAS BEEN DISCONTINUED AND IS INACTIVE*

A library for pure functional Agent-Based Simulation.

This library is being developed during my PhD in which I am exploring the benefits and drawbacks of using the functional programming paradigm in Haskell for implementing Agent-Based Simulations. My research led into an approach I termed Functional Reactive Agent-Based Simulation (FrABS) which I implemented in this library. Due to its hybrid approach (it can easily implement SD and with a bit more work DES) I termed my library 'chimera' after the Chimera from the gree mythology which is also a hybrid being: "[...] usually depicted as a lion, with the head of a goat arising from its back, and a tail that might end with a snakes head [...]" (Wikipedia).

FrABS is a radically different approach to ABS in which agents are implemented as time-continuous signals which can exhibit both continuous and discrete behaviour. Compared to the traditional object-oriented approach FrABS differs fundamentally in the following points:
- FrABS combines both the strength of continuous SD-like and discrete ABS-like approaches: it is continuous time but with discrete events and heterogenous entities. This allows e.g. to implement a SD model much more easier in an agent-based approach and due to continuous-time it results in the SD dynamics using only one run (provided enough agents are used and time-sampling frequency is high enough).
- In FrABS agents are treated as time-continuous signals - if time does not advance then the agent's output stays constant (depending of course on the model, as there are obviously models in which agents do not depend on time).
- In FrABS data-flow is can only be explicit and deterministic - this means that repeated runs are guaranteed to be ALWAYS the same as all sources of non-determinism are eliminated (random-number generators are controlled, sampling-time is fixed and the agents cannot perform any form of side-effects).
- So far agents can only interact with each other indirectly through a shared environment or directly through immutable shared-nothing messages. This is in the spirit of the Actor model and the original idea of object-orientation as conceived by Alan Kay and requires to think agent-interaction very differently than in oo where method-calls are available.
- In addition to messaging which is kind of asynchronous (as it takes 2 steps for a round-trip) I introduced the concept of conversations in which 2 agents can interact with each other directly without time advancing (it is only available in Sequential stepping because only in this case make the semantics sense).
- Sequential and Parallel stepping of the simulation. Sequential means that agents are updated after another where agents updated after can see the actions of agents after. Parallel means that agents are updated virtually at the same time where actions of all agents are only visible to all in the next step. Due to the referential transparency, lack of side-effects and explicit data-flow it is very easy to implement these steppings without having an impact on how an agent must treat its data meaning we can run models with both steppings and compare their result without changing the agents (of course when we need conversations then we need to use sequential but this is then due to some mututal exclusive and synchronised data-access specified in our model and thus our model is not applicable to Parallel).
- Recursive ABS easily possible. Although there exists basically no research and only one paper on recursive simulation () we implemented a prototype approach which allows agents to make actions and project their actions into the future by running the simulation an arbitrary number of steps. Due to the referential transparency, lack of side-effects and explicit data-flow it is very easy to implement recursive ABS as it maps so naturally on the already recursive nature of pure functional programming in Haskell.

At the moment much refactoring is going on as the current prototype is still too much object-oriented and not functional reactive enough. Currently the following points are open
- Transition from Yampa to Dunai / BearRiver
- Applying to simple DES problems e.g. multiple servers with a single queue
- Is there an alternative to the current agent-interaction mechanisms of messaging and conversations?

How-To run examples
cd to examples
cabal sandbox init
cabal install ../../chimera/
cabal run AgentZero (or any other example)
