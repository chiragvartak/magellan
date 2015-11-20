{-# LANGUAGE OverloadedStrings #-}

module Moves.Perft
    ( perft
    , perft_root
    ) where

import Moves.HelperFunctions
import Moves.Board
import Moves.MakeMove
import Moves.KingMoves
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt, intToDigit, isDigit)

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

perft_max_depth = 4

-- Takes as inputs Position and whose turn it is to move and returns the number of 'moves' from that Position.
-- A 'move' is defined by final position at a certain depth resulting from a path.
-- Hence, the same final position reached by a seperate path is counted as a seperate move.
-- Call with depth=0, perft_move_counter=0
perft :: Position -> Bool -> Int -> Int -> IO Int
perft pos white_to_move depth perft_move_counter = do
    --putStr "1"
    if (depth < perft_max_depth)
    then do
        --putStr "2"
        let
            moves = if( white_to_move )
                then possible_moves_w pos
                else possible_moves_b pos
        --putStrLn ""
        --print (C.length moves)
        loop_i 0 pos white_to_move depth moves perft_move_counter
    else do
        return perft_move_counter

-- Takes as inputs Position and whose turn it is to move and returns the number of 'moves' from that Position.
-- Also prints the number of 'moves' for each next immediate move.
perft_root :: Position -> Bool -> Int -> Int -> Int -> IO Int
perft_root pos white_to_move depth perft_move_counter perft_move_total_counter = do
    --putStr "1"
    if (depth < perft_max_depth)
    then do
        --putStr "2"
        let
            moves = if( white_to_move )
                then possible_moves_w pos
                else possible_moves_b pos
        --putStrLn ""
        --print (C.length moves)
        loop_j 0 pos white_to_move depth moves perft_move_counter perft_move_total_counter
    else do
        --print perft_move_counter
        return perft_move_total_counter

-- Initially call with i=0
loop_i :: Int -> Position -> Bool -> Int -> ByteString -> Int -> IO Int
loop_i i pos white_to_move depth moves perft_move_counter = do
    if(i < C.length moves)
    then do
        --print moves
        --print (slice moves i (i+4))
        let
            wpt = make_move (wp pos) (slice moves i (i+4)) 'P'
            wnt = make_move (wn pos) (slice moves i (i+4)) 'N'
            wbt = make_move (wb pos) (slice moves i (i+4)) 'B'
            wrt = make_move (wr pos) (slice moves i (i+4)) 'R'
            wqt = make_move (wq pos) (slice moves i (i+4)) 'Q'
            wkt = make_move (wk pos) (slice moves i (i+4)) 'K'
            bpt = make_move (bp pos) (slice moves i (i+4)) 'p'
            bnt = make_move (bn pos) (slice moves i (i+4)) 'n'
            bbt = make_move (bb pos) (slice moves i (i+4)) 'b'
            brt = make_move (br pos) (slice moves i (i+4)) 'r'
            bqt = make_move (bq pos) (slice moves i (i+4)) 'q'
            bkt = make_move (bk pos) (slice moves i (i+4)) 'k'
            ept = make_move_ep (wp pos .|. bp pos) (slice moves i (i+4))

            wrt2 = make_move_castle wrt (wk pos .|. bk pos) (slice moves i (i+4)) 'R'
            brt2 = make_move_castle brt (wk pos .|. bk pos) (slice moves i (i+4)) 'r'

            (cwkt, cwqt, cbkt, cbqt) = (cwk pos, cwq pos, cbk pos, cbq pos)

            start = (digitToInt $ C.index moves (i))*8 + (digitToInt $ C.index moves (i+1))
            (cwkt2, cwqt2, cbkt2, cbqt2) = if( isDigit $ C.index moves (i+3) )      --regular move
                then
                    if( (shiftL (1::Word64) start) .&. (wk pos) /= 0 )
                        then (False, False, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (bk pos) /= 0 )
                        then (cwkt, cwqt, False, False)
                    else if ( (shiftL (1::Word64) start) .&. (wr pos) .&. ( shiftL (1::Word64) 63 ) /= 0 )
                        then (False, cwqt, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (wr pos) .&. ( shiftL (1::Word64) 56 ) /= 0 )
                        then (cwkt, False, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (br pos) .&. ( shiftL (1::Word64) 7 ) /= 0 )
                        then (cwkt, cwqt, False, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (br pos) .&. (1::Word64) /= 0 )
                        then (cwkt, cwqt, cbkt, False)
                    else (cwkt, cwqt, cbkt, cbqt)
                else (cwkt, cwqt, cbkt, cbqt)

        -- perft_move_counter_mod_3
        pmcm3 <- if( ( ( wkt .&. unsafe_for_white wpt wnt wbt wrt2 wqt wkt bpt bnt bbt brt2 bqt bkt ) == 0 && white_to_move ) ||
                    ( ( bkt .&. unsafe_for_black wpt wnt wbt wrt2 wqt wkt bpt bnt bbt brt2 bqt bkt ) == 0 && (not white_to_move) ) )
            then do
                let
                    perft_move_counter_mod = if (depth+1 == perft_max_depth)
                        then (perft_move_counter+1)
                        else perft_move_counter
                perft (Position wpt wnt wbt wrt2 wqt wkt bpt bnt bbt brt2 bqt bkt ept cwkt2 cwqt2 cbkt2 cbqt2)
                        (not white_to_move) (depth+1) perft_move_counter_mod
            else
                return perft_move_counter

        -- Looping
        loop_i (i+4) pos white_to_move depth moves pmcm3

    else return perft_move_counter

loop_j :: Int -> Position -> Bool -> Int -> ByteString -> Int -> Int -> IO Int
loop_j j pos white_to_move depth moves perft_move_counter perft_move_total_counter = do
    if(j < C.length moves)
    then do
        --print moves
        --print (slice moves j (j+4))
        let
            wpt = make_move (wp pos) (slice moves j (j+4)) 'P'
            wnt = make_move (wn pos) (slice moves j (j+4)) 'N'
            wbt = make_move (wb pos) (slice moves j (j+4)) 'B'
            wrt = make_move (wr pos) (slice moves j (j+4)) 'R'
            wqt = make_move (wq pos) (slice moves j (j+4)) 'Q'
            wkt = make_move (wk pos) (slice moves j (j+4)) 'K'
            bpt = make_move (bp pos) (slice moves j (j+4)) 'p'
            bnt = make_move (bn pos) (slice moves j (j+4)) 'n'
            bbt = make_move (bb pos) (slice moves j (j+4)) 'b'
            brt = make_move (br pos) (slice moves j (j+4)) 'r'
            bqt = make_move (bq pos) (slice moves j (j+4)) 'q'
            bkt = make_move (bk pos) (slice moves j (j+4)) 'k'
            ept = make_move_ep (wp pos .|. bp pos) (slice moves j (j+4))

            wrt2 = make_move_castle wrt (wk pos .|. bk pos) (slice moves j (j+4)) 'R'
            brt2 = make_move_castle brt (wk pos .|. bk pos) (slice moves j (j+4)) 'r'

            (cwkt, cwqt, cbkt, cbqt) = (cwk pos, cwq pos, cbk pos, cbq pos)

            start = (digitToInt $ C.index moves (j))*8 + (digitToInt $ C.index moves (j+1))
            (cwkt2, cwqt2, cbkt2, cbqt2) = if( isDigit $ C.index moves (j+3) )      --regular move
                then
                    if( (shiftL (1::Word64) start) .&. (wk pos) /= 0 )
                        then (False, False, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (bk pos) /= 0 )
                        then (cwkt, cwqt, False, False)
                    else if ( (shiftL (1::Word64) start) .&. (wr pos) .&. ( shiftL (1::Word64) 63 ) /= 0 )
                        then (False, cwqt, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (wr pos) .&. ( shiftL (1::Word64) 56 ) /= 0 )
                        then (cwkt, False, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (br pos) .&. ( shiftL (1::Word64) 7 ) /= 0 )
                        then (cwkt, cwqt, False, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (br pos) .&. (1::Word64) /= 0 )
                        then (cwkt, cwqt, cbkt, False)
                    else (cwkt, cwqt, cbkt, cbqt)
                else (cwkt, cwqt, cbkt, cbqt)

        -- perft_move_counter_mod_3
        pmtc_mod_2 <- if( ( ( wkt .&. unsafe_for_white wpt wnt wbt wrt2 wqt wkt bpt bnt bbt brt2 bqt bkt ) == 0 && white_to_move ) ||
                    ( ( bkt .&. unsafe_for_black wpt wnt wbt wrt2 wqt wkt bpt bnt bbt brt2 bqt bkt ) == 0 && (not white_to_move) ) )
            then do
                let
                    perft_move_counter_mod = if (depth+1 == perft_max_depth)
                        then (perft_move_counter+1)
                        else perft_move_counter
                pmcm3 <- perft (Position wpt wnt wbt wrt2 wqt wkt bpt bnt bbt brt2 bqt bkt ept cwkt2 cwqt2 cbkt2 cbqt2)
                        (not white_to_move) (depth+1) perft_move_counter_mod
                putStrLn $ C.unpack ( move_to_algebra (slice moves j (j+4)) ) ++ " " ++ (show pmcm3)
                let pmtc_mod = perft_move_total_counter + pmcm3
                return pmtc_mod
            else
                return perft_move_total_counter

        -- Looping
        loop_j (j+4) pos white_to_move depth moves 0 pmtc_mod_2

    else return perft_move_total_counter

-- Slice the ByteString from index m to n, n exclusive
slice :: ByteString -> Int -> Int -> ByteString
slice str m n = C.drop m (C.take n str)

move_to_algebra :: ByteString -> ByteString
move_to_algebra move
  | (isDigit $ C.index move 2) = C.pack  [ char_add (C.index move 1) 49
                                      , intToDigit ( fromEnum '8' - fromEnum (C.index move 0) )
                                      , char_add (C.index move 3) 49
                                      , intToDigit ( fromEnum '8' - fromEnum (C.index move 2) )
                                      ]
  | (C.index move 2 == 'E') = "EPmo"
  | otherwise = "PPro"

-- Addition and subtraction of Chars
char_add :: Char -> Int -> Char
char_add ch n = ( toEnum (fromEnum ch + n) ) :: Char
