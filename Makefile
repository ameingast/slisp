CABAL      	= cabal
RUNHASKELL 	= runhaskell
GHCI       	= ghci
MAIN     		= src/Main.hs
INCLUDE 		= src
DEFAULT  		= test
TARGET   		= slisp
TEST				= src/script/test
BINARY			= dist/build/$(TARGET)/$(TARGET)

all: $(DEFAULT)

configure:
	@$(CABAL) configure

build: configure
	@$(CABAL) build

doc: configure
	@$(CABAL) haddock --executables

dist:	configure
	@$(CABAL) sdist

repl: build
	@$(BINARY) -r

ghci:
	@$(GHCI) -i$(INCLUDE) $(MAIN)

clean:
	@$(CABAL) clean

test: build
	@$(TEST)