module SLISP.Test(test) where

import SLISP.Core
import SLISP.Data

tests :: [String]
tests   = ["test/core.lisp"]

runTests ::  [String] -> (SymbolTable,E) -> IO (SymbolTable,E)
runTests [] (t,e) = return (t,e)
runTests (x:xs) (t,_) = do 
  (t',e') <- evalFile t x 
  if fromLispBool e'
    then runTests xs (t',e')
    else error $ "test failed: " ++ x
                                                    
test :: [String] -> IO ()
test files = do
    (t,_) <- loadLibs files (emptyTable,I 1)
    runTests tests (t, I 1)
    return ()
