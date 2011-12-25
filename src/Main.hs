module Main(main) where
    
import SLISP.Core
import SLISP.Data
import SLISP.Repl
import SLISP.Test

import System(getArgs)

main :: IO ()
main =  do
  getArgs >>= \args -> case args of 
    "-h":_ -> putStrLn $ "slisp [-rht] <files>"
    "-r":files -> repl files
    "-t":files -> test files
    files -> loadLibs files (emptyTable, I 1) >> return ()