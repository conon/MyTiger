MyTiger
=======

MyTiger is a compiler which implements the Tiger programming language. 
Tiger was developed by Andrew Appel for his "Modern Compiler Implementation in ML".
I am mainly using MyTiger as a learning and experimenting tool. 


## Features 

* Makes a flawless lexical, syntactic and semantic analysis
 
 * Implicitly produces intermediate representation language (defined in the book)

 * Makes the classic liveness analysis and register allocation

 * Produces ARM assembly for ARM v7 processors


## Known issues

* When there are not enough registers for a function the compilation simply fails and terminates

* For some programs the compiler produces (slightly) wrong assembly

* Negative numbers do not work


## Compilation

MyTiger was tested on cubieboard3 which runs an ARM Cortex-A7 processor with Debian OS. In order to compile MyTiger you need a mlton compliler installed. To compile MyTiger run

mlton -output <output_name> mlton.mlb


## Working examples

I have coded some samples with Tiger language which the MyTiger is confirmed to produce working ARM assembly. They can be found the ./working_examples. For example run
(if the output_name is tiger)
./tiger working_examples/online_insertion_sort.tig
./working_examples/online_insertion_sort.tig.exe

Keep in mind that a runtime.o object file should be in CWD of the source file.
If you want to recompile runtime.c then run
gcc -S -arm runtime.c 
as -g runtime.s -o runtime.o
and move runtime.o to the directory which the tiger source file resides


## Future

I am planning to split MyTiger into 3 branches which are going to,
* Fix the current implementation
* Keep the current front-end and make an interpreter implementation of the language
* Keep the current front-end and use the LLVM for IR

