## The Bazaar Implementation

*Todo* everytihng

*Available* the basic spec

For now,

```
$ cd scribblings
$ scribble --dest Bazaar bazaar.scrbl
$ open Bazaar/bazaar.html
```

This opens the game description (and works as is on macOS only).

1. Plan
2. Equations 
3. Game State
4. Strategy
5. Rules
6. Games
7. Clean Up
8. Observer
9. Remote
10. Revise!

### Milestones

- we release the game description
- we release the component description

#### Plan

- students write up a plan for implementing the game 
- students write up questions about the game

- we release rough semester plan 

#### Equations

- students implement the basic game pieces & functionality 
  - equations:
  - functionality: which equations are usable with the player's current pebbles
  - functionality: visualize equations 
  - cards:
  - functionality: which cards can the player buy with its current pebbles 
  - functionality: visualize cards 
  - also needed, they need to discover: represent pebbles

#### Game State

The game state includes the player's wallet; the bank's pebbles; and
the currently displayed cards, and the scores of all other
players. (It does not include the equations because they remain
constant across the entire game.) 

- students implement
  - what the referee knows about the game (omniscient game state)
  - what the referee shares with a player when it is its turn (public game state)
  - functionality: translate omniscient to public
  - functionality: visualize game state 
  
- deliver test script and tests for equations functionality 

- we cross-test equations functionality 

- partner switch after due date 

#### Strategy

The general strategy is to answer three questions:
 1. should the player roll the die? only if no trade is possible. 
 2. what kind of trades should the player request?
 3. what does the player wish to purchase? 

- students implement
  - game tree for all possible sequences of trades and buys at each node
  - two evaluation functions for nodes
    - maximize the number of points a player can make during this turn
    - maximize the number of cards a player can buy during this turn 

- deliver test script and tests for game state functionality

- we cross-test game state functionality

- we supply the logical protocol for a referee-player interaction. 

#### Rules

- students implement
  - a data representation for the actions a player may request 
  - rule checker
  - scoring function 

- deliver test script and tests for strategy functionality

- we cross-test strategy functionality

#### Games

- students implement
  - referee that runs the game and eliminates ill-behaved players 
  - player mechanism; abstract over and employ strategy properly 
  - and their interaction 

At this point, students should be able to run complete games using their sw.

- deliver test script and tests for rule checking functionality

- we cross-test rule-checking functionality

#### Clean up

- the usual 

- deliver test script and tests for running complete games with well-behaved players 

- we cross-test running games 

- partner switch after due date 

#### Observer or Interactive

- students implement
  - referee observer
  - player observer

- deliver test script and tests for running complete games with exn-raising players 

- we cross-test running games with such players 

#### Remote

- students use observer pattern to implement
  - remote proxies
  - server
  - client

- deliver test script and tests for running complete games with looping players 

- TAs inspect visualization of games

- we cross-test running games with players that behave, raise exns, loop 

#### Revised!

- students revise the code
  - change constants (3 -> 4)

- deliver test script and tests for running remote games with all kinds of players
  - the tests can specify which variant of the rules we run 

- we cross-test the distributed implementation
