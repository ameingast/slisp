module Main(main) where
    
import SLISP.Data
import SLISP.Eval
import SLISP.Parser
import SLISP.Repl
import SLISP.Test

import qualified Control.Exception as E
import qualified Data.Map as M
import Maybe
import System.IO
import System(getArgs)

main :: IO ()
main =  do
  getArgs >>= \args -> case args of 
    "-h":_ -> putStrLn $ "slisp [-rh] <files>"
    "-r":files -> repl files
    files -> loadLibs (libs ++ files) (emptyTable, I 1) >> return ()