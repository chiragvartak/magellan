{-# LANGUAGE OverloadedStrings #-}

module Moves.BoardDebug where

import Moves.KingMoves
import Moves.HelperFunctions
import Moves.Board
import TimingDebug
import Data.Bits
import qualified Data.ByteString.Char8 as C

-- Just a chess board that can be used for debugging
vboard1 =   ['r', '-', '-', '-', 'k', '-', '-', 'r'
            ,'-', '-', 'p', '-', '-', 'p', 'P', '-'
            ,'-', '-', '-', '-', 'p', 'n', '-', '-'
            ,'-', '-', 'P', 'p', '-', '-', 'P', 'p'
            ,'P', 'p', '-', 'P', '-', '-', '-', '-'
            ,'-', 'b', 'B', '-', 'P', 'N', '-', '-'
            ,'-', 'P', 'Q', '-', '-', 'P', 'p', 'P'
            ,'R', '-', '-', '-', 'K', 'B', '-', 'q']

vboard2 =   ['-', '-', '-', '-', '-', '-', '-', '-'
            ,'-', '-', '-', '-', '-', '-', '-', '-'
            ,'-', '-', 'k', '-', '-', '-', '-', '-'
            ,'r', '-', '-', '-', '-', '-', '-', '-'
            ,'-', '-', '-', '-', '-', 'K', '-', '-'
            ,'-', '-', '-', '-', 'R', '-', '-', '-'
            ,'-', '-', '-', '-', '-', '-', '-', '-'
            ,'-', '-', '-', '-', '-', '-', '-', '-']

-- Position constructed from the above vboard 
pos1 = vboard_to_pos vboard1
pos2 = vboard_to_pos vboard2

-- A function for converting the visual board into a Postion datatype. Will be useful for debugging.
vboard_to_pos :: [Char] -> Position
vboard_to_pos vboard = 
    let vboard_modified = zip vboard [0..] -- A vboard that is a list of tuples like ('P', 54)
    in Position { history = "cx470000"::ByteString
                , wp = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'P']
                , wn = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'N']
                , wb = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'B']
                , wr = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'R']
                , wq = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'Q']
                , wk = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'K']
                , bp = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'p']
                , bn = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'n']
                , bb = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'b']
                , br = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'r']
                , bq = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'q']
                , bk = int_list_to_bitboard [snd tuple | tuple <- vboard_modified, fst tuple == 'k'] 
                , ep = 0
                , cwk = False
                , cwq = True
                , cbk = True
                , cbq = True
                }

-- Helper function for above
-- Takes the a list of the bit-numbers and sets them to 1, thereby generating a bitboard
int_list_to_bitboard :: [Int] -> Word64
int_list_to_bitboard xs = foldl setBit 0 xs

-- print a bitboard in a 8x8 view
print66 :: Word64 -> IO ()
print66 bitboard = print65 bitboard 0

print65 :: Word64 -> Int -> IO ()
print65 bitboard 64 = return ()
print65 bitboard bit = do
    let get_bit bitboard bit = (shiftR bitboard bit) .&. 1
    if (bit `mod` 8 == 7)
    then do
        putStrLn $ (show $ get_bit bitboard bit) ++ " "
        print65 bitboard (bit+1)
    else do
        putStr $ (show $ get_bit bitboard bit) ++ " "
        print65 bitboard (bit+1)

-- View a ByteString in groups of 4
-- A good example for looping over each character of a ByteString
view4 :: ByteString -> ByteString
view4 bs =
    if (C.length bs <= 4)
    then
        bs
    else
        (C.take 4 bs) `C.append` " " `C.append` (view4 $ C.drop 4 bs)
        