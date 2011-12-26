module Main(main) where
    
import SLISP.Core
import SLISP.Data
import SLISP.Repl

import System(getArgs)

main :: IO ()
main =  do
  getArgs >>= \args -> case args of 
    [] -> return ()
    "-h":_ -> putStrLn "slisp [-rhi] <files>"
    "-r":files -> repl True files
    "-i":_ -> repl False []
    files -> loadLibs files (emptyTable, I 1) >>= putStrLn . show . snd