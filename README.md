# TinyLISP
Implementation of compiler for TinyLisp to SECD bytecode and SECD machine running said bytecode
## Usage
```make```
creates two binaries ./compiler and ./vm.\
```./run.sh [filename].lisp```\
compiles Tiny lisp code to bytecode and runs it in vm\
```./debug.sh [filename].lisp```\
does the same but runs vm with -v flag to enable debug mode
## Compiler
Implemented in Haskell.\
Reason to do it in Haskell was mainly to try functional language (since this is PPA and not PA) and I already had small knowledge (very small) of it. So any input on how to improve this is very appreciated.\
It is comprised from Lexer, Parser and Abstract syntactic tree.\
Lexer is implemented via simple finate state machine, output is list of tokens. Parser is LL(k) implemented by recusive descent, the number of character I checked differs because I used a lot of patter matching in Haskell. Output from parser is AST which is defined by Expr (expression) data type. After then I generate SECD instruction from AST which are then saved into binary file.
## SECD Machine
Implemented in C++\
With C++ I wanted to try using some thing we were learning in PPA in language I already know and mostly use more modern parts of C++. I'm not sure how well I did but atleast i didnt have to learn Rust :) (which was an option too).\
Whole program is basically reading instruction from file (path name takes from command line argument), which are then executed by one big while loop, in which I just check type of instruction and then do necessary steps to execute it.\
Since I try to use modern (or atleast not ancient) features of C++, I mostly use smart pointers by that I mean that everything is std::shared_ptr. For most part this provided GC (kinda) so I was hoping that would be that to memory managment, unfortunately I found out that shared pointer does not play nice with cycles (which I create when patching dummy enviroment) and that creates leak. At the end I didn't have time to fix this so using ```letrec``` creates leaks but anything else does not (I hope) create leaks :(.
## Language features
I implement mostly identical language to what is in slides for SECD lecture and most features are shown in examples (tests directory). Biggest difference in comparison to "full" lisp is absence of ```define```, for defining functions I mostly use ```letrec```.