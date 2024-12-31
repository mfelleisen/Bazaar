## Test Code for Project Milestone 10, 2024

### Files for test fest, like those for 9

- `xtest` is the testing script 
- `xserver` 
- `xclients`
- `xclients-bonus`

**WRITE ACCESS NEEDED**

Running `xtest` will create the files 

- `n-*.json` in `Tests/` so that the test harness finds all test cases.
  This files should disappear automtaically. 

- `port-starter-file.rktd` in $HOME/Tmp, which records the port last used, 
  just in case something goes wrong with the students' TCP port allocation.
  It is safe to delete this file after a run.  

### Generating Tests 

The tests are generated via `xserver`. Just run it in drracket. 

To do so, it is necessary to set pass `#false` to `clients` in
`xclients`. Setting up this way ensures that the unit test succeeds
and writes its inputs/expecteds to the specified files. 

To run the tests, it is necessary to set pass `#true` to `clients` in
`xclients`. If not, `xclients` shuts down and seems to take down some
of its threads too. 

The Boolean flag dictates whether the script waits for its spwaned
threads or not. 

I consider this a flaw. There's workable fix (communicate between `xerver`
and `xclients`) and there must be an elegant fix (but I haven't figured it
out yet.)

**ALSO**

It is also curious that to run these tests I have to increase the time-out
for calls 8 fold over a very similar test in Bazaar/Server/server. Something
weird is going on here, and I re-discover this every single year. 


### Test Directories:

- `Tests/Clients/` are standard test setups, pretty much like those for `9`
- `Tests/Server/` are standard test setups, pretty much like those for `9`
  except that the referee awards bonus points now 
- `Tests/*Bonus/` are non-standard client setups:
  - players that sign up with bad names
  - players that send broken JSON, plus
  - 2 weird clients that don't follow the registration protocol properly 
  
### Running the required tests

`xtest` should be run as follows: 

```
$ ./xtest pathToMytests/ pathto_MY_client pathto_Their_server 
```
which will test their server with "everything goes well" tests

```
$ ./xtest pathToMytests/ pathto_THEIR_client pathto_MY_server 
```
which will test their clients with "everything goes well" tests

```
$ ./xtest pathToMytests/ pathto_MY_xclients-bonus pathto_THEIR_server bonus 
```
which will test their servers with "clients that violate the protocols
in all kinds of ways" 
