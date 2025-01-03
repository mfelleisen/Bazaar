## Library Files 

this library provides functionality that should probably (or may) exist in Racket's libraries	 	    

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [bags.rkt](bags.rkt) | a generic bad (multi-set) representation | 
| [check-message.rkt](check-message.rkt) | a primitive addition to rackunit that checks whether an expression prints an error message | 
| [configuration.rkt](configuration.rkt) | support for defining objects | 
| [fixed-perm.rkt](fixed-perm.rkt) | make a permutation of a list in fixed order | 
| [json.rkt](json.rkt) | for generating serializing and deserializing plain values from and to JSexpr | 
| [parse-json.rkt](parse-json.rkt) | functionality for deserialzing JSexpr into internal data representations | 
| [require-2-provide.rkt](require-2-provide.rkt) | a mechasnism for importing and re-exporting libraries | 
| [sequence.rkt](sequence.rkt) | a function that checks orderings on equally sequences of X given an ordering on X | 
| [tie-breaking.rkt](tie-breaking.rkt) | collect candidates and realize tie-breaking functionality | 
| [xsend.rkt](xsend.rkt) | a library that protects calls from exceptions and overly slow clients | 

