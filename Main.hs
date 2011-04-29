module Main where
    
import Data
import Eval
import Maybe
import Parser
import qualified Data.Map as M
import System.IO

prompt  = "? "
name    = "S(low)-LISP"
libs    = ["lib/list.lisp"]
tests   = ["test/list.lisp"]

safeLine    ::  String -> IO (String)
safeLine s  =   catch getLine (\_ -> return s)

file        ::  SymbolTable -> String -> IO (SymbolTable,E)
file t f    =   readFile f >>= \x -> return $ last $ listEval (t, parse' x)

loadLibs                ::  [String] -> (SymbolTable,E) -> IO (SymbolTable,E)
loadLibs [] (t,e)       =   return (t,e)
loadLibs (x:xs) (t,e)   =   file t x >>= loadLibs xs

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

main :: IO ()
main =  do
    (t,_) <- loadLibs libs (emptyTable,I 1)
    x <- getStdin ""
    putStrLn $ show $ snd $ last $ listEval (t,parse' x)

getStdin    ::  String -> (IO String)
getStdin s  =   safeLine s >>= \x -> if x == s then return x else getStdin $ s ++ x

repl :: IO ()
repl =  do  
    putStrLn $ "Welcome to " ++ name
    (t,_) <- loadLibs libs (emptyTable,I 1)
    statefulRepl t

statefulRepl   :: SymbolTable -> IO ()
statefulRepl t =  do  
    putStr prompt
    hFlush stdout
    x <- safeLine ":q"
    case x of
        ""          ->  statefulRepl t
        ":q"        ->  return ()
        ":r"        ->  putStrLn "---" >> repl
        ':':'e':x   ->  
            let cmds = if x == []   then unwords $ map fst $ M.toList t 
                                    else show $ fromJust $ M.lookup (tail x) t
            in  do  putStrLn cmds
                    statefulRepl t
        _           ->  
            let (t',e) = last $ listEval (t,parse' x)
            in  do  putStrLn $ show e
                    statefulRepl t'