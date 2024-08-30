## Referee 

this component implements the Bazaar referee 

The referee is organized as a mechanism that sets up an initial state,
consults external players to cause transitions from one state
to the next, and applies a rule chhecker to each such transition
until a final state is reached. 

### Basic Concepts and Relationships

```
+---------+
| Referee |
+---------+              +-----------+
| State s |------------->| GameState | (between two turns)
+---------+              +-----------+
                         | Players   |----------------+
                         | + Actors  |                |
                         | Bank      | ---------------------------------+
                         | Visibles  |                |                 |
                         | Cards     |                |* QUEUE          |
                         +-----------|                |                 |
                                                      |                 |
                                                  +----------+          |
                                                  | Player   |          v
                                                  +----------+       +-----+
                                                  | Wallet   | ----> | Bag |
                                                  | Score    |       +-----+
                                                  +----------+  
```

### Basic Functionality 

The referee creates an initial game state and then proceeds as follows: 

1. it sets up the actors with the equations and connects them to the players in the game state 

2. it grants each actor a complete turn
   - kicks it out if it fails/cheats at any stage during the turn 
   - updates the game state if the actor requests legitimate actions and rotates the queue 
     - legality checking simultaneously creates a new game state, including a score 
   - inform all registered observers of the new game state

3. repeat step 2 or stop the game if the game state is a final one.

4. it informs the surviving players of their status as winners or losers.
   - inform all registered observers of the final outcome 

### Referee and Observers

Some external code unit creates observers in a new EventSpace and
hands them to a newly created referee (together with other data, say,
a player or states). 

```
    External
      | 
      | 
      |    new()
  o = | ------------> Observer() ...
  o = | ------------> Observer() ...
  o = | ------------> Observer() ...
  o = | ------------> Observer() ...
      |                   |
      |                   |
      |                   |
      |           new(... o ...)
  r = | ----------------------------------------------> Referee()
      |                   |                            |
      .                   |   state(...)               | 
      .                   | <------------------------- | %% + a state for each legal turn action
      .                   .                            .
      |                   .                            .
      |                   .                            .                               
      |                   |                            |
      |                   |   state(...)               |
      |                   | <------------------------- | %% until the game ends 
      |                   |                            |    
      |                   |                            |
      |                   |   end(...)    	       |
      |                   | <------------------------- |
      |                   | 			       |
      |                   |			       _  %% ref shuts down 
      |    show()         |			      ___ 
      | ----------------> |			      
      |                   | %% user interacts 
      |                   |			      
      |                   | %% user quits
      |			  _ 
      |			 ___ %% observer shuts down 
      |
      _
     ___ %% external shuts down 
```

### Details: Referee 

The referee keeps track of ill-behaved players so that once they
perform any bad action, they are never contacted again.

To discover ill-behaved players, the referee protects _all_ calls to players with

```
 (xsend player method-name arg ...)
```     
so that it catches

- exn-s raised internally
- can terminate overly long calls.

It manages calls to observers via an instance of manage-observers.
This object loops over all registered observers and uses `xsend` to
inform them.

The referee's game state suffices to resume a game at turn
bondaries. (Indeed, one could suspend in the middle of a turn and
resume from there.)

The module comes with one entry point:

- referee/state, which consumes a set of equations, a bunch of actors, and a preconfigured state plus optional observers

### Details: Observer

The observers are a stateful objects. When an observer receives
information about a state changes, it caches it. When it receives the
end of game signal, it prepares the chached information for inspection
by a user.


### Details: Managing Observer

Register observers and inform them by `xsend` ing information to them.

#### TODO

The loop should remove (unregister) observers that fail.

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [game-state.rkt](game-state.rkt) | the referee's game state representation, including "connections" to the actual players | 
| [manage-observers.rkt](manage-observers.rkt) | a tool for managing observes via looping over registered observers and `xsend` | 
| [observers.rkt](observers.rkt) | .. allow the referee to `send state` messages and `send end` messages to signal that a game is over | 
| [referee.rkt](referee.rkt) | the referee: a state machine that sets up a GameState, iterates over it by granting turns, | 



