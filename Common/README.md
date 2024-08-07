## Common 

this component represents the common ontology of Q players and the Q gaming framework 

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [player-interface.rkt](player-interface.rkt) | a player interface that the referee can use to service players | 
| [turn-state.rkt](turn-state.rkt) | a data representation of the information that the referee sends to the active player | 
| [player.rkt](player.rkt) | a data representation of the ref's knowledge about the active olayer that it shares during turn | 
| [README.more](README.more) |  | 
| [actions.rkt](actions.rkt) | a data representation of the actions a player may request | 
| [bags.rkt](bags.rkt) | bags filled with pebbles | 
| [cards.rkt](cards.rkt) | a data representation of cards | 
| [equations.rkt](equations.rkt) | a data representation of (generic) equations | 
| [pebbles.rkt](pebbles.rkt) | a data representationn for pebbles | 
| [rule-book.rkt](rule-book.rkt) | the rules of the Bazaar game: legality and scoring | 


Here is a rough overview of the layers: 

```
 KEY CONCEPTS
 
        + ---------------- +
        | player-interface |
        + ---------------- +
        | - setup          |         + ---------------- +        + ---------------- +
        | - take-turn      | ------> | IAction          | -----> | trade->purchase  | 
        | - win            |         + ---------------- +        + ---------------- +
        | - name           |                |        + ---------------- +
        + ---------------- +                + -----> | want-pebble      |
                                                     + ---------------- +
        + ---------------- +    *     + ---------------- +          
        | turn-state       | -------> | player           | 
        + ---------------- + ---+     + ---------------- +         
        | active           |    |     | score            |
        | wallet           |    |     | pebbles          |
        | bank             |    |     + ---------------- +
        | cards            |    |
        | scores           |    |     
        + ---------------- +    |
                                |
        ---------------------------------------------------------------------------------------------------
                                |
 BASIC CONCEPTS                 |
                                |
          +---------------------+---------------------+
          |                     |                     |
 + ---------------- +     + ---------------- +   + ---------------- +     
 | Equations/1Eq    |     | Cards            |   | Pebbles          |
 + ---------------- +     + ---------------- +   + ---------------- +
          |                     |
          + ------------------- +
                      |
                + ---------------- +      
                | Bags             |
                + ---------------- +
```
### Generic Game State and Interaction Protocol 

```


-----------------------------------------------------------------------------

   game-state                     referee                         player (p_n) 
 
        |    extract()               |                                |
        | < ------------------------ |                                |
        |    turn-state: ts          |                                | 
        | ======================== > |                                | 
        |                            |   take-turn(ts)                | 
        |                            | -----------------------------> | 
        |                            |                                |

case 1: 

        |                            |  trade->purchase(t, c)         |
        |                            | <============================= |
        |                            |                                |
        | trade->purchase(t, c)      |                                |
        | <------------------------- |                                |
        |   #false or game-state?    |                                |
        | =========================> |                                |

case 2: 

        |                            |     want-pebble                |
        |                            | <============================= |
        |  want-pebble-okay?         |                                |
        | < ------------------------ |                                |
        |                            |                                |
        |   #false or pebble?        |                                |
        | =========================> |                                |
        |                            |                                |

if pebble?:

        |                            |  take-turn(turn-state+)        | 
        |                            | -----------------------------> |
        |                            |                                |
        |                            |  trade->purchase([], c)        |
        |                            | <============================= |
        |                            |                                |
        | trade->purchase([], c)     |                                |
        | <------------------------- |                                |
        |   #false or game-state?    |                                |
        | =========================> |                                |
        
if any callback returns #false:

        | kick-active-player()       |                                
        | <------------------------- |
        |                            | 
        | state-rotate()             | 
        | <------------------------- |                                
        
otherwise:

        |                            | 
        | state-rotate()             | 
        | <------------------------- |                                
```

### The Logical Protocol 

#### Starting the Game 

```

referee                         player (p_1) . . . player (p_n)
  |                                |                 |
  |                                |                 |
  |                                |                 |
  |     setup(equations)    	   |                 | % the table of equations
  | -----------------------------> |                 | 
  |                                |                 | 
  .                                .                 .
  .                                .                 . % repeat for descending age
  .                                .                 . 
  |                                |                 |
  |     setup(equations)    	   |                 | 
  | -----------------------------------------------> |
  |                                |                 |
```

#### Turn Case 1: player requests a random pebble

```

referee                         player (p_1) . . . player (p_n)
  |                                |                 |
  |   take-turn(TurnState)         |                 | % player receives:
  | -----------------------------> |                 | % - turn state            
  |                                |                 |
  |     WANT-PEBBLE                |                 | % requests a pebble 
  | <============================  |                 |
  |                                |                 | 
  |--+                             |                 |
  .  |                             .                 . % if legal:
  .  |                             .                 . % referee modifies game state
  .  |                             .                 . % completes turn 
  .  |                             .                 . % otherwise: 
  .  |                             .                 . % kick player out 
  .<-+                             .                 . % completes turn 

  IF LEGAL:
  | -----------------------------> |                 | 
  |   take-turn(TurnState)         |                 | % player receives:
  | -----------------------------> |                 | % - turn state
  |                                |                 | % with additional pebble
  |   trade->purchase([],cards)    |		     | 
  | <============================= |                 | % purchases cards
  |--+                             |                 |
  .  |                             .                 . % if legal:
  .  |                             .                 . % referee modifies game state
  .  |                             .                 . % completes turn 
  .  |                             .                 . % otherwise: 
  .  |                             .                 . % kick player out 
  .<-+                             .                 . % completes turn 
  
```

#### Turn Case 2: player trades and purchases only

```

referee                         player (p_1) . . . player (p_n)
  |                                |                 |
  |   take-turn(TurnState)         |                 | % player receives:
  | -----------------------------> |                 | % - turn state            
  |                                |                 |   
  |   trade->purchase(eqs,cards)   |		     | % trade by equations
  | <============================= |                 | % purchases cards
  |--+                             |                 |
  .  |                             .                 . % if legal:
  .  |                             .                 . % referee modifies game state
  .  |                             .                 . % completes turn 
  .  |                             .                 . % otherwise: 
  .  |                             .                 . % kick player out 
  .<-+                             .                 . % completes turn 
  
```

#### Ending the Game

```

referee                        player (p_1) . . . player (p_n)
  |                                |                 |
  |                                |                 |
  |     win(Boolean)               |                 | 
  | -----------------------------> |                 | % true means "winner"
  |                                |                 | % false means "loser" 
  .                                .                 . 
  .                                .                 . 
  .                                .                 .
  .                                .                 .
  |     win(Boolean)               |                 |
  | -----------------------------------------------> | % both winners and 
  |                                |                 | % losers are informed 
  |                                |                 |
```
