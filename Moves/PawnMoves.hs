{-# LANGUAGE OverloadedStrings #-}

module Moves.PawnMoves
    ( possibleWP2
    ) where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt)

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- Both the functions posiblePW and posiblePW2 are almost exact in time complexity.
-- Done timing analysis.

-- This function generates the possible moves for white pawns
-- Incomplete function posiblePW. Use posiblePW2 instead.
possibleWP :: Position -> [String]
possibleWP pos = 
    let not_white_pieces = complement (wp pos .|. wn pos .|. wb pos .|. wr pos .|. wq pos .|. wk pos .|. bk pos)
        black_pieces = bp pos .|. bn pos .|. bb pos .|. br pos .|. bq pos
        empty = complement ((wp pos) .|. (wn pos) .|. (wb pos) .|. (wr pos) .|. (wq pos) .|. (wk pos) .|. (bp pos) .|. (bn pos)
             .|. (bb pos) .|. (br pos) .|. (bq pos) .|. (bk pos))

        pawn_moves_top_right = (shiftR (wp pos) 7) .&. black_pieces .&. (complement rank_8) .&. (complement file_a)
        move1 i = show (i`div`8 + 1) ++ show (i`mod`8 - 1) ++ show (i`div`8) ++ show (i`mod`8)
        list1 = [move1 i | i <- [0..63], (shiftR pawn_moves_top_right i).&.1 == 1]

        pawn_moves_top_left = (shiftR (wp pos) 9) .&. black_pieces .&. (complement rank_8) .&. (complement file_h)
        move2 i = show (i`div`8 + 1) ++ show (i`mod`8 + 1) ++ show (i`div`8) ++ show (i`mod`8)
        list2 = [move2 i | i <- [0..63], (shiftR pawn_moves_top_left i).&.1 == 1]

        pawn_moves_1_forward = (shiftR (wp pos) 8) .&. empty .&. (complement rank_8)
        move3 i = show (i`div`8 + 1) ++ show (i`mod`8) ++ show (i`div`8) ++ show (i`mod`8)
        list3 = [move3 i | i <- [0..63], (shiftR pawn_moves_1_forward i).&.1 == 1]
        
        pawn_moves_2_forward = (shiftR (wp pos) 16) .&. empty .&. (shiftR empty 8) .&. rank_4
        move4 i = show (i`div`8 + 2) ++ show (i`mod`8) ++ show (i`div`8) ++ show (i`mod`8)
        list4 = [move4 i | i <- [0..63], (shiftR pawn_moves_2_forward i).&.1 == 1]

        -- Functions for pawn moves by promotion are below.
        -- Pawn promotion by capturing top right
        pawn_promotion_top_right = (shiftR (wp pos) 7) .&. black_pieces .&. rank_8 .&. (complement file_a)
        move5 i = show (i`mod`8-1) ++ show (i`mod`8) ++ "QP"
                    ++ show (i`mod`8-1) ++ show (i`mod`8) ++ "RP"
                    ++ show (i`mod`8-1) ++ show (i`mod`8) ++ "BP"
                    ++ show (i`mod`8-1) ++ show (i`mod`8) ++ "NP"
        list5 = [move5 i | i <- [0..63], (shiftR pawn_promotion_top_right i).&.1 == 1]

        pawn_promotion_top_left = (shiftR (wp pos) 9) .&. black_pieces .&. rank_8 .&. (complement file_h)
        move6 i = show (i`mod`8+1) ++ show (i`mod`8) ++ "QP"
                    ++ show (i`mod`8+1) ++ show (i`mod`8) ++ "RP"
                    ++ show (i`mod`8+1) ++ show (i`mod`8) ++ "BP"
                    ++ show (i`mod`8+1) ++ show (i`mod`8) ++ "NP"
        list6 = [move6 i | i <- [0..63], (shiftR pawn_promotion_top_left i).&.1 == 1]

        pawn_promotion_1_forward = (shiftR (wp pos) 8) .&. empty .&. rank_8
        move7 i = show (i`mod`8) ++ show (i`mod`8) ++ "QP"
                    ++ show (i`mod`8) ++ show (i`mod`8) ++ "RP"
                    ++ show (i`mod`8) ++ show (i`mod`8) ++ "BP"
                    ++ show (i`mod`8) ++ show (i`mod`8) ++ "NP"
        list7 = [move7 i | i <- [0..63], (shiftR pawn_promotion_1_forward i).&.1 == 1]

        list = list1 ++ list2 ++ list3 ++ list4 ++ list5 ++ list6 ++ list7

    in list

-- This function generates the possible moves for white pawns
possibleWP2 :: ByteString -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> ByteString
possibleWP2 history wp bp not_white_pieces black_pieces empty = 
    let
        pawn_moves_top_right = (shiftR wp 7) .&. black_pieces .&. (complement rank_8) .&. (complement file_a)
        move1 i = (C.pack $ show $ i`div`8+1) `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list1 = C.concat [move1 i | i <- [0..63], (shiftR pawn_moves_top_right i).&.1 == 1]

        pawn_moves_top_left = (shiftR wp 9) .&. black_pieces .&. (complement rank_8) .&. (complement file_h)
        move2 i = (C.pack $ show $ i`div`8+1) `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list2 = C.concat [move2 i | i <- [0..63], (shiftR pawn_moves_top_left i).&.1 == 1]

        pawn_moves_1_forward = (shiftR wp 8) .&. empty .&. (complement rank_8)
        move3 i = (C.pack $ show $ i`div`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list3 = C.concat [move3 i | i <- [0..63], (shiftR pawn_moves_1_forward i).&.1 == 1]
        
        pawn_moves_2_forward = (shiftR wp 16) .&. empty .&. (shiftR empty 8) .&. rank_4
        move4 i = (C.pack $ show $ i`div`8+2) `C.append` (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list4 = C.concat [move4 i | i <- [0..63], (shiftR pawn_moves_2_forward i).&.1 == 1]

        -- Functions for pawn moves by promotion are below.
        -- Pawn promotion by capturing top right
        pawn_promotion_top_right = (shiftR wp 7) .&. black_pieces .&. rank_8 .&. (complement file_a)
        move5 i = (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "QP" 
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "RP" 
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "BP" 
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "NP" 
        list5 = C.concat [move5 i | i <- [0..63], (shiftR pawn_promotion_top_right i).&.1 == 1]

        pawn_promotion_top_left = (shiftR wp 9) .&. black_pieces .&. rank_8 .&. (complement file_h)
        move6 i = (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "QP"
                    `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "RP"
                    `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "BP"
                    `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "NP"
        list6 = C.concat [move6 i | i <- [0..63], (shiftR pawn_promotion_top_left i).&.1 == 1]

        pawn_promotion_1_forward = (shiftR wp 8) .&. empty .&. rank_8
        move7 i = (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`mod`8) `C.append` "QP"
                    `C.append` (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`mod`8) `C.append` "RP"
                    `C.append` (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`mod`8) `C.append` "BP"
                    `C.append` (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`mod`8) `C.append` "NP"
        list7 = C.concat [move7 i | i <- [0..63], (shiftR pawn_promotion_1_forward i).&.1 == 1]

        -- en passant right
        is_2_steps_move = ((C.length history) >= 4) &&
                                    (C.index history (C.length history - 1) == C.index history (C.length history - 3)) &&
                                    (digitToInt (C.index history (C.length history - 2)) - digitToInt (C.index history (C.length history - 4)) `elem` [-2,2])
        e_file = digitToInt (C.index history (C.length history - 1))
        -- shows the piece to remove, not the destination
        en_passant_right_bb = (shiftL wp 1) .&. bp .&. (rank_5) .&. (complement file_a) .&. (file_masks_8 !! e_file)
        list8 = if (is_2_steps_move==True && en_passant_right_bb/=0)
                then
                    let
                        move i = (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`div`8-1) `C.append` (C.pack $ show $ i`mod`8)
                        list = C.concat [move i | i <- [0..63], (shiftR en_passant_right_bb i).&.1 == 1]
                    in list
                else
                    C.empty

        -- en passant left
        -- shows the piece to remove, not the destination
        en_passant_left_bb = (shiftR wp 1) .&. bp .&. (rank_5) .&. (complement file_h) .&. (file_masks_8 !! e_file)
        list9 = if (is_2_steps_move==True && en_passant_left_bb/=0)
                then
                    let
                        move i = (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`div`8-1) `C.append` (C.pack $ show $ i`mod`8)
                        list = C.concat [move i | i <- [0..63], (shiftR en_passant_left_bb i).&.1 == 1]
                    in list
                else
                    C.empty

        list = list1 `C.append` list2 `C.append` list3 `C.append` list4 `C.append` list5 `C.append` list6 `C.append` list7 `C.append` list8 `C.append` list9

    in list
