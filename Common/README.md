## Common 

this component represents the common ontology of Q players and the Q gaming framework 

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [turn-state.rkt](turn-state.rkt) | a data representation of the information that the referee sends to the active player | 
| [player.rkt](player.rkt) | a data representation of the ref's knowledge about the active olayer that it shares during turn | 
| [bags.rkt](bags.rkt) | bags filled with pebbles | 
| [cards.rkt](cards.rkt) | a data representation of cards | 
| [equations.rkt](equations.rkt) | a data representation of (generic) equations | 
| [pebbles.rkt](pebbles.rkt) | a data representationn for pebbles | 


Here is a rough overview of the layers: 

```
 KEY CONCEPTS
 
        + ---------------- +
        | player-interface |
        + ---------------- +
        | - setup          | refers to `states`, `coordinates`
        | - take-turn      |
        | - new-tiles      |
        | - win            |
        | - name           |
        + ---------------- +
        
        + ---------------- +    *     + ---------------- +          
        | turn-state       | -------> | players          | 
        + ---------------- + ---+     + ---------------- +         
        | players          |    |     | score            |
        | tiles            |    |     | pebbles          |
        | legal            |    |     + ---------------- +
        | score            |    |     
        | active-*         |    |
        + ---------------- +    |
                                |
        ---------------------------------------------------------------------------------------------------
                                |
 BASIC CONCEPTS                 |
                                |
```

