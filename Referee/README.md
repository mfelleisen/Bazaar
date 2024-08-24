## Referee 

this component implements the Q referee 

The referee is organized as a mechanism that sets up an initial state,
consults external players to cause transitions from one state
to the next, and applies a rule chhecker to each such transition
until a final state is reached. 

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [game-state.rkt](game-state.rkt) | the referee's game state representation, including "connections" to the actual players | 
| [referee.rkt](referee.rkt) | the referee: a state machine that sets up a GameState, iterates over it by granting turns, | 



