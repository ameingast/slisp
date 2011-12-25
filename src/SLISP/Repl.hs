module SLISP.Repl(repl) where
  
import SLISP.Core
import SLISP.Data
import SLISP.Eval
import SLISP.Parser
import SLISP.Util.IO(safeLine)

import qualified Control.Exception as E
import qualified Data.Map as M

import Maybe
import System.IO

prompt :: String
prompt = "? "

banner :: String
banner = "S(low)-LISP"

repl :: [String] -> IO ()
repl files = do  
    putStrLn $ "Welcome to " ++ banner
    (t,_) <- loadLibs files (emptyTable,I 1)
    safeStatefulRepl t

safeStatefulRepl :: SymbolTable -> IO ()
safeStatefulRepl t = E.catch (statefulRepl t) $ handler t

handler :: SymbolTable -> E.SomeException -> IO ()
handler t e = do
  putStrLn $ "Exception: " ++ show e
  safeStatefulRepl t

statefulRepl :: SymbolTable -> IO ()
statefulRepl t =  do  
    putStr prompt
    hFlush stdout
    x <- safeLine ":q"
    putStrLn x
    case x of
        "" -> statefulRepl t
        ":q" -> return ()
        ":r" -> putStrLn "---" >> repl []
        ':':'e':x' ->  
            let cmds = if x' == [] then unwords $ map fst $ M.toList t 
                                   else show $ fromJust $ M.lookup (tail x') t
            in putStrLn cmds >> statefulRepl t
        _ ->  
            let (t',e) = last $ listEval (t,parse' x)
            in putStrLn (show e) >> statefulRepl t'
