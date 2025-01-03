## Clients

this component directory implements the client code for the Bazaar game. 

The client program consumes a list of players and spawns a
client thread for each of them, as if they were running on separate
machine.

Each such thread TCP-connects to the Bazaar server on a specified port.
Once the connection is accepted, it signs up the player with its given
name.

At that point, the client thread delegates to a "proxy-referee", which
acts like a referee from the player's perspective.

The implementation uses a general library for calling a server and
specifying the connectivity (call-me vs call-you)

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [client.rkt](client.rkt) | a client that signs up some players with a server at a given IP address and port, | 
| [referee.rkt](referee.rkt) | the proxy referee runs a player in the same context as a referee proper | 


