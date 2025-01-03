## Players 

this component implements a Bazaar player.

A player implements the mechanism that employs "strategies" to compute
responses. For Bazaar, a strategy is a pair of functions:

- from the current state of the game to a request for a random pebble or a sequence of trades
- from the state with the revised set of pebbles to a sequence of card purchases 

Some player implementations realize 

- cheaters, which is the normal misbehavior in a game;
- exception raising ones, which represents misbehavior of local components;
- time-consuming ones, which represents misbehavior in distributed settings.

### Concepts and Relationships

```
+----------------+
|  Player        |
+----------------+               +--------------------------------------------+ 
| Strategy       | ------------> | Strategy                                   |
+----------------+               +--------------------------------------------+ 
| setup          |               | should-the-player-request-a-random-pebble  |
| pebbleOrTrades |               | trade-then-purchase                        |
| cards?         |               | buy-cards                                  |
| win            |               +--------------------------------------------+
+----------------+               
     ^
     |
     | via macros and mixins 
     | 
     |
  +--+------------------------+-----------------------------+
  |                           |                             |
+----------+               +----------+                 +----------+
| ExnPlyr  |               | InfPlyr  |                 | Cheats   |
+----------+               +----------+                 +----------+
```

```
                                  Player
                                    |
                                    |    new (maximize)
                                    | ------------------------> Strategy
                                    |                            |
         pebbbleOrTrades(turnState) |                            |
        --------------------------> |                            |
                                    |   should-the-...()         |
                                    | -------------------------> |
                                    |                            |
                                    |   boolean                  |
           yes, if #true            | <========================  |
        <=========================  |                            |
                                    |                            |
                                    |                            |
                                    |                            |
         cards?(turnState)          |                            |
        --------------------------> |                            |
                                    |                            |
                                    |                            |
                                    |   buy-cards()              |
                                    | -------------------------> |
                                    |                            |
                                    |                            |
                                    |   sequence of cardss       |
                                    | <========================= |
                                    |                            |
                                    |                            |
           sequence of cards        |                            |                                  
        <=========================  |                            |


                if #false:

                                    |                            |
                                    |   trade-then-purchase()    |
                                    | -------------------------> |
                                    |                            |
                                    |   exchanges-plus-cards     |
                                    | <========================  |
           exchange requests        |                            |                                  
        <=========================  |                            |
                                    |                            |
                                    |                            |
         cards?(turnState)          |                            |
        --------------------------> |                            |
                                    |                            |
           sequence of cards        |                            |                                  
        <=========================  |                            |
                                    |                            |



```


### Functionality

The mechanism is a functional component.

A strategy is always a function. Even a history-sensitive variant
should consume a data representation of the game's history.  

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [mechanism.rkt](mechanism.rkt) | a class representation of the player mechanism, including subclasses whose methods | 
| [strategies.rkt](strategies.rkt) | two strategies for a Bazaar player for choosing | 

