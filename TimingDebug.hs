module TimingDebug where

import Text.Printf
import Control.Exception
import System.CPUTime
import Control.Parallel.Strategies
import Control.Monad
import System.Environment
import Control.DeepSeq (deepseq)

-- Timing an IO Action
-- Just run 'time_io <IO Action> <repititions>' in ghci
-- Performs an IO Action 'repetitions' number of times
-- Pretty sure that there is a problem with this method due to lazy evaluation
time_io :: IO t -> Integer -> IO ()
time_io a repetitions = do
	putStrLn "Starting ..."
	start <- getCPUTime
	v <- time_io_helper a repetitions
	print v
	end <- getCPUTime
	putStrLn "Done."
	let diff = (fromIntegral (end - start)) / (10^9)
	printf "Computation time: %0.3f ms\n" (diff :: Double)
	putStrLn ""

-- Takes value and returns the time required to compute that value.
-- Put the 'value' the thing that you want to evaluate; can even be a function call.
time_pure :: (NFData a) => a -> IO ()
time_pure value = do
	putStrLn "Starting ..."
	start <- getCPUTime
	let r = value
	end <- r `deepseq` getCPUTime
	putStrLn "Done."
	let diff = (fromIntegral (end - start)) / (10^9)
	printf "Computation time: %0.3f ms\n" (diff :: Double)

time_pure' :: (NFData a) => a -> Int -> IO ()
time_pure' value iter = do
	putStrLn "Starting ..."
	start <- getCPUTime
	-- I need to do all the numerous computations here
	-- Call with dummy_count = 0
	let r = time_pure'_helper iter value 0
	end <- r `deepseq` getCPUTime
	putStrLn "Done."
	let diff = (fromIntegral (end - start)) / (10^9)
	printf "Computation time: %0.3f ms\n" (diff :: Double)

time_pure'_helper :: (NFData a) => Int -> a -> Int -> Int
time_pure'_helper 0 _ dummy_count = dummy_count
time_pure'_helper iter value dummy_count =
	let
	iter_mod = iter - 1
	dummy_count_mod = (value) `deepseq` (dummy_count+1)
	in time_pure'_helper iter_mod value dummy_count_mod

-- A benchmarking function for timing pure functions
{-
benchmark :: [String] -> IO Integer
benchmark inputList = do
                     start <- getCPUTime
                     let r = foo inputList
                     end <- r `deepseq` getCPUTime
                     return (end - start)
-}

time_io_helper :: IO t -> Integer -> IO String
time_io_helper a 0 = return "If printed then repitions done!\n"
time_io_helper a repetitions = do
	v <- a
	time_io_helper a (repetitions-1)

main = do
	putStrLn "Eric. Charles."

-- Timing a pure computation
-- Convert the pure computation into an IO action using 'return'
