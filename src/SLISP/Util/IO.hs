module SLISP.Util.IO (safeLine) where

import System.Console.Haskeline(runInputT, defaultSettings, getInputLine)
  
safeLine :: String -> IO (String)
safeLine s = runInputT defaultSettings (getInputLine "") >>= \x -> case x of
  Just s' -> return s'
  Nothing -> return s