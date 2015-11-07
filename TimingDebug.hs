module TimingDebug where

import Text.Printf
import Control.Exception
import System.CPUTime
import Control.Parallel.Strategies
import Control.Monad
import System.Environment

-- Timing an IO Action 
-- Just run 'time_io <IO Action>' in ghci
-- Performs an IO Action 'repetitions' number of times
time_io :: IO t -> Integer -> IO ()
time_io a repetitions = do
	putStrLn "Starting ..."
	start <- getCPUTime
	time_io_helper a repetitions
	end <- getCPUTime
	putStrLn "Done."
	let diff = (fromIntegral (end - start)) / (10^9)
	printf "Computation time: %0.3f ms\n" (diff :: Double)
	return ()

time_io_helper :: IO t -> Integer -> IO ()
time_io_helper a 0 = return ()
time_io_helper a repetitions = do
	v <- a
	time_io_helper a (repetitions-1)
	
main = do
	putStrLn "Eric. Charles."

-- Timing a pure computation
-- Convert the pure computation into an IO action using 'return'
