# S(low) LISP

## About
SLISP is a (very) basic lisp interpreter written in Haskell.

It supports function definitions, lambda-expressions and has a couple of builtin operators.
In addition to that, it has a (very) basic list library implemented in lisp.

## Dependencies
* GHC 6
* Parsec

## Usage
To start it, run:

    > echo "(+ 1 1)" | runhaskell Main.hs
