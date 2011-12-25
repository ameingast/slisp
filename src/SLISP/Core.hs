module SLISP.Core(loadLibs, evalFile) where

import SLISP.Data
import SLISP.Eval
import SLISP.Parser

libs :: [String]
libs = ["lib/core.lisp"]
  
evalFile :: SymbolTable -> String -> IO (SymbolTable,E)
evalFile t f = readFile f >>= \x -> return $ last $ listEval (t, parse' x)

loadLibs :: [String] -> (SymbolTable,E) -> IO (SymbolTable,E)
loadLibs xs st = loadLibs' (libs ++ xs) st
  where
    loadLibs' [] (t,e) = return (t,e)
    loadLibs' (x:xs') (t,_) = do
      evalFile t x >>= loadLibs' xs'