module SLISP.Util.System (
  time
) where

import System.CPUTime(getCPUTime)
  
time :: IO (t) -> IO (t)
time f = do
    start <- getCPUTime
    x <- f
    end <- getCPUTime
    let diff = end - start
    putStrLn $ "Computation time: " ++ show diff
    return x