## lambda-calculus-interpreter
A simple lambda calculus interpreter. 


#### Cabal and Fourmolu setup

To use the Fourmolu pre-commit hook, install the version listed in `.fourmolu-version` (currently `cabal install fourmolu-0.21.0.0`), then enable the hook with `git config core.hooksPath .githooks`. To build the project and load it on the cli with the same command use `cabal repl`.


#### De Bruijn indices

To represent a lambda expression we use **De-Bruijn indices**: each variable is either a free variable, in this case it is represented by a string, or it is a bound variable, in this case is represented by an integer which indicates how "close" the abstraction to which it is linked is. 

This makes **alpha equivalence** straight forward, since it is just about having the same representation tree and having the same free and bound variables. 

**Reduction** is a little bit trickier: when reducing we must keep into account that some of the indices might need to be shifted. Let's assume we are reducing `Eapp (Eabs "x" Pippo) Pluto` which is the subset of an expression. If we remove the abstraction, each of the variable indices inside `Pippo` bound to higher abstractions in the tree must be shifted by one. Another shift regards `Pluto`, since when substituting it inside `Pippo` it might end up having additional abstractions as ancestors, so all the variables inside `Pluto` bound to outer abstractions must be shifted.


#### Parser




#### Problems and solutions 

Since we where having so many problems with our labeling strategy, we looked online for a better representation of expressions and we found De Bruijn indices which solved all of the problems we had, making alpha equivalence way more intuitive and solving all the bugs of the label.

Another mistake we made in the beginning: we thought that application had higher priority than abstraction, so this is one of the last things we fixed.

The project was developed completely independently so one of the hardest part was understanding how to build expressions such as the predecessor (`exprPrec`) and the factorial (`exprFact`). For the former we eventually looked for an hint after spending many hours on it: the idea of pairing church numerals is brilliant but we didn't manage to come up with it by ourselves. For the logic we had to look for the standard representation of `true` and `false`.
