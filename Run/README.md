## Running a Server or Client 

how to run the system from either the server side or the client side (or both) 

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [get.rkt](get.rkt) | a function for retrieving a possibly optional JSON value from STDIN | 
| [run-server-client.rkt](run-server-client.rkt) | values common to xserver and xclients | 
| [sample-client-config.json](sample-client-config.json) |  | 
| [sample-server-config.json](sample-server-config.json) |  | 


### Run Server

```
$ ./xserver port-number < server-configuration 
```

A _server-configuration_ is one of:

- empty                      :: no bonuses are awared after all turns have been played. 
- contains just "rwb"        :: a bonus is awarded to every player that posses card(s) displaying red white and blue pebbles
- contains just "seychelles" :: a bonus is awarded to every player that posses card(s) displaying all differently colored pebbles

**Example** To run a server from the top-level repo: 

```
$ .Run/xserver 12345 < ./Run/sample-server-config.json
```

This configuration file is empty, meaning no bonuses are awarded. 

### Run Client

```
$ ./xclients port-number [optional-ip-address] < sample-client-config.json
```

When no ip-address is given, the clients are pointed to a local-host server at the specified port. 


A _client-configuration_ is a file that contains a single JSON array.
Each value in the array is an array of two values: `[Name, Strategy]`.

A _Name_ is a non-empty JSON string that consists of at most 20 alphanumeric chars.

A _Strategy_ is one of:

- "purchase-size"
- "purchase-points"

**Example** To run two clients from the top-level repo:

```
$ .Run/xserver 12345 < ./Run/sample-client-config.json
```

This configuration file specifies two players, each playing with a different strategy. 
