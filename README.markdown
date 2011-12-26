S(LOW) LISP
===========
SLISP is an interpreter for a side-effect free LISP dialect written in Haskell.

It supports lambda-expressions, various atom datatypes, maps and more.

Language
--------
    Program     :: (Atom | List | FuncExpr | QuotedExpr)*
      
    Atom        :: Number | Symbol | String | Key
    
    List        :: '(' Expression+ ')'
    
    Expression  :: Atom | List | FuncExpr | QuotedExpr
    
    FuncExpr    :: '#' Expression

    QuotedExpr  :: ''' Expression

Usage
-----
**REPL**: Run _make repl_ to build the executable and launch SLISP in repl 
mode with the core library loaded.

**Test Suite**: Run _make test_ to launch the SLISP test suite.

Requirements
------------
* Cabal 1.6 or newer
* GHC 6 or newer