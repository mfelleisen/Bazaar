## The Bazaar Project

### Install

```
$ raco pkg install git://github.com/mfelleisen/SwDev.git
$ git clone git://github.com/mfelleisen/Bazaar.git
$ cd Bazaar
$ raco pkg install
$ raco doc Bazaar
```

[ This last command may fail on occasion. If so, try `raco doc universe`. If this command also fails, it is a problem with Racket's document installation. ] 


### Run

```
$ ./xtest
```

runs the tests for the _logical_ part of the system.

```
$ raco test Server/server.rkt
```

runs the tests for the server/client part of the system. 

See [Run](Run/README.md)

#### Generate Readme

To re-generate the README file, run

```
$ ./xreadme --help
```

then run as desired. 

The repository is a fully functioning game framework, intended to be uses
as both an educational and a research project.

### Read 

#### Table of Content 

The following table describes the purpose of each directory in this repository.
For detailed explanations of the files, follow the links. 


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| directory | purpose |
|--------------------- | ------- |
| [Referee](Referee/README.md) | this component implements the Bazaar referee | 
| [Common](Common/README.md) | This component represents the common ontology of Bazaar players and the Bazaar gaming framework. | 
| [Client](Client/README.md) | this component directory implements the client code for the Bazaar game. | 
| [Lib](Lib/README.md) | this library provides functionality that should probably (or may) exist in Racket's libraries | 
| [Player](Player/README.md) | this component implements a Bazaar player. | 
| [Remote](Remote/README.md) | this component support the implementationn of a remote-proxy protocol, | 
| [Server](Server/README.md) | this component implements the Bazaar server. | 

| file | purpose |
|--------------------- | ------- |
| [homework.rkt](homework.rkt) | this file exists to export names for the milestone specifications of the Sw Dev course | 
| [xreadme](xreadme) | generate the README.md files from README.source files, *.txt files, and purpose statements | 
| [xtest](xtest) | run `raco test` on all relevant code files | 


#### The Idea 

Bazaar is a table game. Run `raco doc Bazaar`. 

This repository is a framework for programming competitive Bazaar players,
a variant of the original Bazaar game.  Participants design automated
players that run on their desktops and connect to a (remote) Bazaar
server. This server runs a single game. Any misbehavior of a
player---non-responsiveness due to bugs or cheating---results in
immediate termination. So the competition is first about delivering
robust code and second about writing great strategies.

#### Milestones

See [Milestones](https://felleisen.org/matthias/4500-f23/index.html) for details.

 |              DUE |                on this due date |          TITLE |             PURPOSE |
 |             DATE |                	     	      |		       |	     	     |
 |             DATE |                	     	      |		       |	     	     |
 | ---------------- | ------------------------------- | -------------- | ------------------- | 
  |1  Thursday,  12 September 2024 | -thb B -hw 2     | The Plan        | question, sprints, memo style    |
  |2  Wednesday, 25 September 2024 | -hw 3            | Equations       | treaming -- language eval        |
  |3  Tuesday,   01 October   2024 | -hw 4            | The State       | partner eval                     |
  |4  Thursday,  10 October   2024 | -hw 5            | The Strategies  | PARTNER SWITCH 1                 |
  |5  Thursday,  17 October   2024 | -hw 6            | The Rules       |              			   |
  |6  Thursday,  24 October   2024 | -hw 7            | Games!          |                                  |
  |7  Thursday,  31 October   2024 | -hw 8            | The Clean Up    | PRTNER SWITCH 2                  |
  |8  Thursday,  07 November  2024 | -hw 9            | The Observer    | save PNGs, opt. display          |
  |9  Thursday,  14 November  2024 | -hw 10           | Remote          |                                  |
 |10  Thursday,  28 November  2024 |  all done        | Revised!        | change something `deep`          |

The final integration test runs

- students' servers with instructor players
- the instructor's server with a student's players
- plus bonus test that evaluate the robustness of students' servers (broken protocols)


### Concepts and Relationships

```
 +----------------------------+                           +----------------------------+
 | Client                     |                           | Server                     |
 +----------------------------+                           +----------------------------+
 | signup-with-server module  |                           | signing-up players module  |
 | player mechanism           | relies on      relies on  | referees, its game state   |
 | strategies                 |-----------+  +------------| observers                  |
 +----------------------------+           |  |            +----------------------------+
                                          |  |
                                          v  v
                 +---------------------------------------------------------+
                 | Common Ontology (of Clients and Server)                 |
                 +---------------------------------------------------------+
                 | player interface and protocol                           |
                 | the player's game state its own knowledge              |
                 |     plus its knowledge about others                     |
                 | the rules (legality checking)			   |
                 | basic game pieces and constants                         |
                 +---------------------------------------------------------+
```


#### The Common Ontology

For participating AI players to connect to the server and participate in games, they
need to understand the interaction protocol, which specify remote calls, their
signatures, and their proper sequencing.  While each message is just a piece JSON
data, the common ontology provides the proper interpretation of these pieces of data
in context.

Additionally, the players' creators need to know the rules of the game. The rules of
the game are expressed in terms of game states and other games pieces. 

Since even a detailed interpretation may leave questions about their meaning with
respect to the rules of the game. the repository's `Common` directory publishes the
code that interprets the remote calls with respect to the server and the sample
player in this repository.

#### Server Sign-Up

```
server                           client (c_1) ... client (c_i)
  |                                |                 | 
  |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |                 | % tcp connect 
  |                                |                 |
  |   ActorName n_1                |                 | 
  |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |                 | % send a name 
  |                                |                 | % no reply 
  |                                |                 |
  | new(n_1) rpp_1                 |                 |
  |------->+                       |                 | % create remote-proxy player 
  |        |                       |                 |
  |        |                       |                 |
  .        |                       .                 .
  .        |                       .                 .
  .        |                       .                 .
  |        |                       |                 |
  |   ActorName  n_i               |                 | 
  |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ | 
  |        |                       |                 |
  |        |                       |                 |
  | new(n_i)           rpp_i       |                 |
  |-------------------->+          |                 |
  |        |            |          |                 |
  |        |            |          |                 |
  |        |            |          |                 |
  |
  |
  |
  |
  | new(rpp_1,..., rpp_n)   referee                     % create referee to run a game 
  |-------------------------------------+               % with the remote proxies
  |                                     |
  |                                     |
  |                                     |
```

#### The Logical Interaction Protocol

The following sequence diagrams sketch how proxy referee and the proxy
player re-connect the referee and the players over TCP. 

_Starting the Game_

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

_Running Turns_

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

_Ending the Game_

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




