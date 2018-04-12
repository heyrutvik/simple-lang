# simple-lang
SIMPLE language implementation in Scala. It is a toy language which is explained in Tom Stuart's book Understanding Computation.

**Tasks**

- [x] Operational Semantics
	- [x] Small-Step Semantics
	- [x] Big-Step Semantics
- [x] Denotational Semantics
- [x] Parser

**Setup Steps**

- clone this repo
- change directory to the root of cloned repo and enter command ```sbt```
	- to print abstract syntax tree of the program
	```run -ast <program.simple>```
	- to interpret program and see steps and output
	```run -interpret <program.simple>```
	- to compile program in javascript
	```run -compile <program.simple> -o <program.js>```
	
***Note***
- ```<program.simple>``` must be a file in current directory, it doesn't expect file path!
- ```hello.simple``` file is available in root directory to execute above commands.
