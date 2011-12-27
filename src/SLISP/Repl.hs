module SLISP.Repl (
repl
) where
  
import SLISP.Core
import SLISP.Data
import SLISP.Eval
import SLISP.Parser
import SLISP.Util.IO(safeLine)

import qualified Control.Exception as E(catch, SomeException)
import qualified Data.Map as M(toList, lookup)

import Maybe
import System.IO

prompt :: String
prompt = "? "

banner :: String
banner = "S(low)-LISP"

repl :: Bool -> [String] -> IO ()
repl verbose files = do
  if verbose then putStrLn ("Welcome to " ++ banner) else return ()
  (t,_) <- loadLibs files (emptyTable,I 1)
  safeStatefulRepl verbose t

safeStatefulRepl :: Bool -> SymbolTable -> IO ()
safeStatefulRepl verbose t = E.catch (statefulRepl verbose t) $ handler verbose t

handler :: Bool -> SymbolTable -> E.SomeException -> IO ()
handler verbose t e = do
  putStrLn $ "Exception: " ++ show e
  safeStatefulRepl verbose t

statefulRepl :: Bool -> SymbolTable -> IO ()
statefulRepl verbose t =  do  
    putStr $ if verbose then prompt else ""
    hFlush stdout
    x <- safeLine ":q"
    case x of
        "" -> statefulRepl verbose t
        ":q" -> return ()
        ':':'e':' ':x' ->  
            let cmds = if x' == [] then unwords $ map fst $ M.toList t 
                                   else showFun $ fromJust $ M.lookup x' t
            in putStrLn cmds >> statefulRepl verbose t
        _ ->
          case listEval (t, parseLisp x) of
            [] ->  statefulRepl verbose t
            xs -> let (t', e) = last xs
                  in putStrLn (show e) >> statefulRepl verbose t'

-- TODO: write in dot-free notation by converting (a, b) to a -> b
showFun :: ([String], E) -> String
showFun (args, body) = formatFun args body