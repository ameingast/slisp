module SLISP.Test(runTests) where

import SLISP.Data
import SLISP.Eval
import SLISP.Parser
import SLISP.Repl

import Maybe
import System.IO

tests   = ["test/list.lisp"]

runTests                ::  [String] -> (SymbolTable,E) -> IO (SymbolTable,E)
runTests [] (t,e)       =   return (t,e)
runTests (x:xs) (t,e)   =   do  (t',e') <- file t x 
                                if fromLispBool e'  then runTests xs (t',e')
                                                    else error $ "test failed: " ++ x
                                                    
test :: IO ()
test =  do
    (t,_) <- loadLibs libs (emptyTable,I 1)
    runTests tests (t, I 1)
    putStrLn "tests finished"
