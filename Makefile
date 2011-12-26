CABAL      	= cabal
RUNHASKELL 	= runhaskell
GHCI       	= ghci
MAIN     		= src/Main.hs
INCLUDE 		= src
DEFAULT  		= runhaskell
TARGET   		= slisp

all: test

configure:
	@$(CABAL) configure

build: configure
	@$(CABAL) build

doc: configure
	@$(CABAL) haddock --executables

dist:	configure
	@$(CABAL) sdist

run: build
	@./dist/build/$(TARGET)/$(TARGET)

runhaskell:
	@$(RUNHASKELL) -i$(INCLUDE) $(MAIN) -r

repl:
	@$(GHCI) -i$(INCLUDE) $(MAIN)

wc:
	@find src -iname "*.hs" | xargs wc -l

clean:
	@$(CABAL) clean

test: build
	@./src/script/test