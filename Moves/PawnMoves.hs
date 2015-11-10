{-# LANGUAGE OverloadedStrings #-}

module Moves.PawnMoves
    ( possibleWP2
    , possibleBP2
    ) where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt)

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- This function generates the possible moves for white pawns
possibleWP2 :: ByteString -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> ByteString
possibleWP2 history wp bp cannot_capture can_capture empty = 
    let
        pawn_moves_top_right = (shiftR wp 7) .&. can_capture .&. (complement rank_8) .&. (complement file_a)
        move1 i = (C.pack $ show $ i`div`8+1) `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list1 = C.concat [move1 i | i <- [0..63], (shiftR pawn_moves_top_right i).&.1 == 1]

        pawn_moves_top_left = (shiftR wp 9) .&. can_capture .&. (complement rank_8) .&. (complement file_h)
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
        pawn_promotion_top_right = (shiftR wp 7) .&. can_capture .&. rank_8 .&. (complement file_a)
        move5 i = (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "QP" 
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "RP" 
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "BP" 
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "NP" 
        list5 = C.concat [move5 i | i <- [0..63], (shiftR pawn_promotion_top_right i).&.1 == 1]

        pawn_promotion_top_left = (shiftR wp 9) .&. can_capture .&. rank_8 .&. (complement file_h)
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

possibleBP2 :: ByteString -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> ByteString
possibleBP2 history wp bp cannot_capture can_capture empty = 
    let
        pawn_moves_top_right = (shiftL bp 7) .&. can_capture .&. (complement rank_1) .&. (complement file_h)
        move1 i = (C.pack $ show $ i`div`8-1) `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list1 = C.concat [move1 i | i <- [0..63], (shiftR pawn_moves_top_right i).&.1 == 1]

        pawn_moves_top_left = (shiftL bp 9) .&. can_capture .&. (complement rank_1) .&. (complement file_a)
        move2 i = (C.pack $ show $ i`div`8-1) `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list2 = C.concat [move2 i | i <- [0..63], (shiftR pawn_moves_top_left i).&.1 == 1]

        pawn_moves_1_forward = (shiftL bp 8) .&. empty .&. (complement rank_1)
        move3 i = (C.pack $ show $ i`div`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list3 = C.concat [move3 i | i <- [0..63], (shiftR pawn_moves_1_forward i).&.1 == 1]
        
        pawn_moves_2_forward = (shiftL bp 16) .&. empty .&. (shiftL empty 8) .&. rank_5
        move4 i = (C.pack $ show $ i`div`8-2) `C.append` (C.pack $ show $ i`mod`8) `C.append` (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8)
        list4 = C.concat [move4 i | i <- [0..63], (shiftR pawn_moves_2_forward i).&.1 == 1]

        -- Functions for pawn moves by promotion are below.
        -- Pawn promotion by capturing top right
        pawn_promotion_top_right = (shiftL bp 7) .&. can_capture .&. rank_1 .&. (complement file_h)
        move5 i = (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "QP" 
                    `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "RP" 
                    `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "BP" 
                    `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "NP" 
        list5 = C.concat [move5 i | i <- [0..63], (shiftR pawn_promotion_top_right i).&.1 == 1]

        pawn_promotion_top_left = (shiftL bp 9) .&. can_capture .&. rank_1 .&. (complement file_a)
        move6 i = (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "QP"
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "RP"
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "BP"
                    `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`mod`8) `C.append` "NP"
        list6 = C.concat [move6 i | i <- [0..63], (shiftR pawn_promotion_top_left i).&.1 == 1]

        pawn_promotion_1_forward = (shiftL bp 8) .&. empty .&. rank_1
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
        en_passant_right_bb = (shiftR bp 1) .&. wp .&. (rank_4) .&. (complement file_h) .&. (file_masks_8 !! e_file)
        list8 = if (is_2_steps_move==True && en_passant_right_bb/=0)
                then
                    let
                        move i = (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8+1) `C.append` (C.pack $ show $ i`div`8+1) `C.append` (C.pack $ show $ i`mod`8)
                        list = C.concat [move i | i <- [0..63], (shiftR en_passant_right_bb i).&.1 == 1]
                    in list
                else
                    C.empty

        -- en passant left
        -- shows the piece to remove, not the destination
        en_passant_left_bb = (shiftL bp 1) .&. wp .&. (rank_4) .&. (complement file_a) .&. (file_masks_8 !! e_file)
        list9 = if (is_2_steps_move==True && en_passant_left_bb/=0)
                then
                    let
                        move i = (C.pack $ show $ i`div`8) `C.append` (C.pack $ show $ i`mod`8-1) `C.append` (C.pack $ show $ i`div`8+1) `C.append` (C.pack $ show $ i`mod`8)
                        list = C.concat [move i | i <- [0..63], (shiftR en_passant_left_bb i).&.1 == 1]
                    in list
                else
                    C.empty

        list = list1 `C.append` list2 `C.append` list3 `C.append` list4 `C.append` list5 `C.append` list6 `C.append` list7 `C.append` list8 `C.append` list9

    in list
