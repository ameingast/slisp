module SLISP.Repl(repl, loadLibs, file, libs) where
  
import SLISP.Data
import SLISP.Eval
import SLISP.Parser
import SLISP.Util.IO(safeLine)

import qualified Control.Exception as E
import qualified Data.Map as M
import Maybe
import System.IO

prompt  = "? "
libs    = ["lib/list.lisp"]
banner  = "S(low)-LISP"

repl :: [String] -> IO ()
repl files =  do  
    putStrLn $ "Welcome to " ++ banner
    (t,_) <- loadLibs (libs ++ files) (emptyTable,I 1)
    safeStatefulRepl t

safeStatefulRepl :: SymbolTable -> IO ()
safeStatefulRepl t = E.catch (statefulRepl t) handler
  where handler e = putStrLn (show (e::E.SomeException)) >> safeStatefulRepl t

statefulRepl   :: SymbolTable -> IO ()
statefulRepl t =  do  
    putStr prompt
    hFlush stdout
    x <- safeLine ":q"
    case x of
        ""          ->  statefulRepl t
        ":q"        ->  return ()
        ":r"        ->  putStrLn "---" >> repl []
        ':':'e':x   ->  
            let cmds = if x == []   then unwords $ map fst $ M.toList t 
                                    else show $ fromJust $ M.lookup (tail x) t
            in  do  putStrLn cmds
                    statefulRepl t
        _           ->  
            let (t',e) = last $ listEval (t,parse' x)
            in  do  putStrLn $ show e
                    statefulRepl t'
                    
file        ::  SymbolTable -> String -> IO (SymbolTable,E)
file t f    =   readFile f >>= \x -> return $ last $ listEval (t, parse' x)

loadLibs                ::  [String] -> (SymbolTable,E) -> IO (SymbolTable,E)
loadLibs [] (t,e)       =   return (t,e)
loadLibs (x:xs) (t,e)   =   file t x >>= loadLibs xs