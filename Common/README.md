## Common 

This component represents the common ontology of Bazaar players and the Bazaar gaming framework. 

The `player-interface` module serves as a facade interface for the entire module.
It `provide`s a class signature for the referee to call on players, and it
`provide`s access to all public building blocks needed to understand this
building block, including the JSON messaging between the referee on the server
and remote players on distant computers. 

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [player-interface.rkt](player-interface.rkt) | a player interface that the referee can use to service players | 
| [turn-state.rkt](turn-state.rkt) | a data representation of the information that the referee sends to the active player | 
| [player.rkt](player.rkt) | a data representation of the ref's knowledge about the active olayer that it shares during turn | 
| [actions.rkt](actions.rkt) | a data representation of the actions a player may request | 
| [bags.rkt](bags.rkt) | bags filled with pebbles | 
| [cards.rkt](cards.rkt) | a data representation of cards | 
| [equations.rkt](equations.rkt) | a data representation of (generic) equations | 
| [pebbles.rkt](pebbles.rkt) | a data representationn for pebbles | 
| [rule-book.rkt](rule-book.rkt) | the rules of the Bazaar game: legality and scoring | 


Here is a rough overview of the layers: 

```
 KEY CONCEPTS

        +-----------------------------------------------------------------------------+
        |         player-interface (a facade for the entire Common/ component)        |
        +-----------------------------------------------------------------------------+
        |                  |
        | - setup          |         + ---------------- +        + ---------------- +
        | - pebble-or-trade| ------> | IAction          | -----> | exchanges        |
        | - cards?         | ------> |                  | -----> | card purchase    | 
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
### The Remote Protocol

The following sequence diagrams sketch how proxy referee and the proxy
player re-connect the referee and the players over TCP. 

#### Starting the Game 

```

           server side                                   many client sides (*)
-----------------------------------------------------------------------------------
referee                       proxy_player (p_*) //    proxy_ref (p_*)   player p_*
  |                               |              //      |                | 
  |                               |              //      |                | 
  |  setup(s:equations)           |              //      |                | 
  | ----------------------------> |              //      |                |   
  |                               |  (s):JSON    //      |                | 
  |                               | ~~~~~~~~~~~~ // ~~~> |                | 
  |                               |              //      |   setup(s)     | 
  |                               |              //      | -------------> | 
  |                               |              //      |   void         | 
  |                               |  void:JSON   //      | <============  | 
  |    void                       | <~~~~~~~~~~~ // ~~~~ |                | 
  | <============================ |              //      |                | 
  |                               |              //      |                | 
  |                               |              //      |                | 
  .                               .              //      .                . 
  .                               .              //      .                . 
```

#### Running Turns

```

     server side          proxies                    clients
-----------------------------------------------------------------------------------
        
referee                      //     player (p_1) . . . player (p_n)
  |                          //      |                 |
  |                          //      |                 | % call only if game
  |                          //      |                 | % is not finished
  |   requestPebbleortrades( //      |                 | % player receives:
  | ------------------------ // ---> |                 | % - turn state            
  |             TurnState)   //      |                 |
  |                          //      |                 |
  |                          //      |                 |
  |     EitherPebbleOrExchang//es    |                 | % requests a pebble 
  | <======================= // ===  |                 | % or 
  |                          //      |                 | % an exchange of pebbles
  |                          //      |                 |
  |                          //      |                 |
  |                          //      |                 |
  |--+                       //      |                 |
  .  |                       //      .                 . % if legal:
  .  |                       //      .                 . % referee modifies game state
  .  |                       //      .                 . % completes turn 
  .  |                       //      .                 . % otherwise: 
  .  |                       //      .                 . % kick player out 
  .<-+                       //      .                 . % completes turn 
                             //
  IF LEGAL:                  //
  | requestCards(TurnState)  //      |                 | % player receives:
  | ------------------------ // ---> |                 | % - turn state
  |                          //      |                 | % with a revised 
  |                          //      |                 | % wallet of pebbles
  |                          //      |                 |
  |                          //      |                 |
  |   SequenceOf<Card>       //      |                 | 
  | <========================//===== |                 | % purchases cards
  |--+                       //      |                 |
  .  |                       //      .                 . % if legal:
  .  |                       //      .                 . % referee modifies game state
  .  |                       //      .                 . % completes turn 
  .  |                       //      .                 . % otherwise: 
  .  |                       //      .                 . % kick player out 
  .<-+                       //      .                 . % completes turn 
                             //
                             
```

#### Ending the Game

```
  server side              proxies             clients
-----------------------------------------------------------------------------------
referee                              player (p_1) . . . player (p_n)
  |                         //               |                 |
  |                         //               |                 |
  |    win(Boolean)         //               |                 |
  | ---------------- ~~~~   //  ~~~~ ------> |                 | 
  |                         //               |                 | 
  .                         //               .                 . 
  .                         //               .                 . 
  .                         //               .                 . 
  .                         //               .                 .
  |    win(Boolean)         //               |                 |
  | ---------------- ~~~~   //  ~~~~ ------------------------> |
  |                         //               |                 |
  |                         //               |                 |
```

### The Logical Protocol 

#### Starting the Game 

```

referee                         player (p_1) . . . player (p_n)
  |                                |                 |
  |                                |                 |
  |                                |                 |
  |     setup(equations)           |                 | % the table of equations
  | -----------------------------> |                 | 
  |                                |                 | 
  .                                .                 .
  .                                .                 . % repeat for descending age
  .                                .                 . 
  |                                |                 |
  |     setup(equations)           |                 | 
  | -----------------------------------------------> |
  |                                |                 |
```

#### Turn Case 1: player requests a random pebble

```
        
referee                         player (p_1) . . . player (p_n)
  |                                |                 |
  |                                |                 | % call only if game
  |                                |                 | % is not finished
  |   requestPebbleortrades(       |                 | % player receives:
  | -----------------------------> |                 | % - turn state            
  |             TurnState)         |                 |
  |                                |                 |
  |                                |                 |
  |     EitherPebbleOrExchanges    |                 | % requests a pebble 
  | <============================  |                 | % or 
  |                                |                 | % an exchange of pebbles
  |                                |                 |
  |                                |                 |
  |                                |                 |
  |--+                             |                 |
  .  |                             .                 . % if legal:
  .  |                             .                 . % referee modifies game state
  .  |                             .                 . % completes turn 
  .  |                             .                 . % otherwise: 
  .  |                             .                 . % kick player out 
  .<-+                             .                 . % completes turn 

  IF LEGAL:
  |                                |                 |
  |   requestCards(TurnState)      |                 | % player receives:
  | -----------------------------> |                 | % - turn state
  |                                |                 | % with a revised 
  |                                |                 | % wallet of pebbles
  |                                |                 |
  |                                |                 |
  |   SequenceOf<Card>             |                 | 
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
