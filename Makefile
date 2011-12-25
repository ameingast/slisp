CABAL      	= cabal
RUNHASKELL 	= runhaskell
GHCI       	= ghci
MAIN     		= src/Main.hs
INCLUDE 		= src
DEFAULT  		= runhaskell
TARGET   		= Main


all: 	repl

configure:
	@$(CABAL) configure

build: 	configure
	@$(CABAL) build

doc: 	configure
	@$(CABAL) haddock --executables

dist: 	configure
	@$(CABAL) sdist

run: 	build
	@./dist/build/$(TARGET)/$(TARGET)

runhaskell:
	@$(RUNHASKELL) -i$(INCLUDE) $(MAIN) 

repl:
	@cd src && $(GHCI) Main.hs

wc:
	@find src -iname "*.hs" | xargs wc -l

clean:
	@$(CABAL) clean
