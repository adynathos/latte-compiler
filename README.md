# Latte Compiler Exercise

This is a compiler of the Java-like *Latte* language, as an exercise in compiler design.
The parser is generated by [BNF Converter](https://bnfc.digitalgrammars.com/).
LLVM is used to build the binaries.

* [The grammar](LatekGrammar.cf)
* [Example program](programs/shapes.lat)

**Features:**

* classes with properties and methods
* type checking
* control flow (if, while), with [short-circuit condition evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation)
* compilation to native binaries through LLVM

