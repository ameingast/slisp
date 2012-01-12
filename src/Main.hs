module Main(
  main,
  emptyRepl
) where
    
import SLISP.Core
import SLISP.Data
import SLISP.Repl
import SLISP.Util.System(time)

import System(getArgs, exitFailure)
import qualified Control.Exception as E(catch, SomeException)

main :: IO ()
main = do
  getArgs >>= \args -> case args of 
    [] -> return ()
    "-h":_ -> putStrLn "slisp [-rhib] <files>"
    "-r":files -> repl True files
    "-i":_ -> repl False []
    "-b":files -> time (runFiles files)
    files -> runFiles files

emptyRepl :: IO ()
emptyRepl = repl True []

runFiles :: [String] -> IO ()
runFiles files = E.catch (justRun files) handler

justRun :: [String] -> IO ()    
justRun files = loadLibs files (emptyTable, Fixnum 1) >>= putStrLn . show . snd

handler :: E.SomeException -> IO ()
handler e = (putStrLn $ "Exception: " ++ show e) >> exitFailure