module SLISP.Core (loadLibs, evalFile) where

import SLISP.Data
import SLISP.Eval
import SLISP.Parser

libs :: [String]
libs = ["src/lib/core.lisp"]
  
evalFile :: State -> String -> IO (State)
evalFile (t, _) f = readFile f >>= \x -> return $ last $ listEval (t, parseLisp x)

loadLibs :: [String] -> State -> IO (State)
loadLibs xs st = loadLibs' (libs ++ xs) st
  where
    loadLibs' [] st' = return st'
    loadLibs' (x:xs') st' = evalFile st' x >>= loadLibs' xs'