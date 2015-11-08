-- This GHC extension makes the string literals polymorphic.
-- This makes it easy to use ByteStrings. 
{-# LANGUAGE OverloadedStrings #-}

module Board where

import qualified Data.Word
import Data.Bits
-- Just import the Data.ByteString.Char8 interface.
-- No need to import Data.ByteString as well. Char8 has it all.
-- ByteStrings are more efficient than Strings in Haskell, wrt both space and time complexity
-- Hence we will be using ByteStrings in place of Strings.
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt)

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- This construct contains all the information required to represent a state completely.
-- Does it?
data Position = Position { history :: ByteString, wp :: Word64, wn :: Word64, wb :: Word64, wr :: Word64, wq :: Word64, wk :: Word64
                        , bp :: Word64, bn :: Word64, bb :: Word64, br :: Word64, bq :: Word64, bk :: Word64
                        } deriving (Show)

-- Things required for the posiblePW function below
file_a  =  72340172838076673::Word64
file_h  =  9259542123273814144::Word64
file_ab  =  217020518514230019::Word64
file_gh  =  13889313184910721216::Word64
rank_8  =  255::Word64
rank_5 = 4278190080::Word64
rank_4 = 1095216660480::Word64
rank_1 = 18374686479671623680::Word64
centre = 103481868288::Word64
extended_centre = 66229406269440::Word64
king_side = 17361641481138401520::Word64
queen_side = 1085102592571150095::Word64
-- not_white_pieces : Pieces that CANNOT be captured by white; includes black king
-- black_pieces : Pieces that CAN be captured by white; doesn't include black king
-- empty : Empty squares

rank_masks_8 = [0xFF, 0xFF00, 0xFF0000, 0xFF000000, 0xFF00000000, 0xFF0000000000, 0xFF000000000000, 0xFF00000000000000]
file_masks_8 = [0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
                0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080]
diagonal_masks_8 = [0x1, 0x102, 0x10204, 0x1020408, 0x102040810, 0x10204081020, 0x1020408102040, 0x102040810204080,
                    0x204081020408000, 0x408102040800000, 0x810204080000000, 0x1020408000000000, 0x2040800000000000,
                    0x4080000000000000, 0x8000000000000000]
antidiagonal_masks_8 = [0x80, 0x8040, 0x804020, 0x80402010, 0x8040201008, 0x804020100804, 0x80402010080402,
                        0x8040201008040201, 0x4020100804020100, 0x2010080402010000, 0x1008040201000000, 0x804020100000000,
                        0x402010000000000, 0x201000000000000, 0x100000000000000]

-- This function gives all the possible moves of white. This is the one you should call.
-- If you want just one, say, pawn moves, comment the rest in this function and debug. 
possible_moves_w :: Position -> ByteString
possible_moves_w pos = let
    not_white_pieces = complement (wp pos .|. wn pos .|. wb pos .|. wr pos .|. wq pos .|. wk pos .|. bk pos)
    black_pieces = bp pos .|. bn pos .|. bb pos .|. br pos .|. bq pos
    empty = complement ((wp pos) .|. (wn pos) .|. (wb pos) .|. (wr pos) .|. (wq pos) .|. (wk pos) .|. (bp pos) .|. (bn pos)
        .|. (bb pos) .|. (br pos) .|. (bq pos) .|. (bk pos))
    occupied = complement empty
    list = --possibleWP2 (history pos) (wp pos) (bp pos) not_white_pieces black_pieces empty
            --possibleWN
            possibleWB occupied (wb pos) not_white_pieces
            --possibleWR
            --possibleWQ
            --possibleWK
    in list

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

-- Some functions required for generating moves for sliding pieces, and functions required for those functions
reverse_bits :: Word64 -> Word64
reverse_bits v = let
    v0 = v
    v1 = ( (shiftR v0 1) .&. 0x5555555555555555 ) .|. ( shiftL (v0 .&. 0x5555555555555555) 1 )
    v2 = ( (shiftR v1 2) .&. 0x3333333333333333 ) .|. ( shiftL (v1 .&. 0x3333333333333333) 2 )
    v3 = ( (shiftR v2 4) .&. 0x0F0F0F0F0F0F0F0F ) .|. ( shiftL (v2 .&. 0x0F0F0F0F0F0F0F0F) 4 )
    v4 = ( (shiftR v3 8) .&. 0x00FF00FF00FF00FF ) .|. ( shiftL (v3 .&. 0x00FF00FF00FF00FF) 8 )
    v5 = ((shiftR v4 16) .&. 0x0000FFFF0000FFFF ) .|. ( shiftL (v4 .&. 0x0000FFFF0000FFFF) 16)
    v6 = ((shiftR v5 32) .&. 0x00000000FFFFFFFF ) .|. ( shiftL (v5 .&. 0x00000000FFFFFFFF) 32)
    in v6

-- h_and_v_moves and d_and_antid_moves are required generate the sliding moves required for generating bishop, rook and queen moves.
h_and_v_moves :: Int -> Word64 -> Word64
h_and_v_moves square occupied = let
    binary_square = shiftL (1::Word64) square
    possibilities_horizontal = (occupied - 2*binary_square) `xor` (reverse_bits (reverse_bits occupied - 2 * reverse_bits binary_square))
    possibilities_vertical = ((occupied .&. (file_masks_8 !! (square `mod` 8))) - (2 * binary_square)) `xor` (reverse_bits (reverse_bits (occupied .&. (file_masks_8 !! (square `mod` 8))) - (2 * reverse_bits binary_square) ))
    in ( ( possibilities_horizontal .&. (rank_masks_8 !! (square `div` 8)) ) .|. possibilities_vertical .&. (file_masks_8 !! (square `mod` 8)) )

d_and_antid_moves :: Int -> Word64 -> Word64
d_and_antid_moves square occupied = let
    binary_square = shiftL (1::Word64) square
    possibilities_diagonal = ( (occupied .&. ( diagonal_masks_8 !! (square`div`8 + square`mod`8) ) ) - (2*binary_square) )
                                `xor` reverse_bits ( reverse_bits ( occupied .&. (diagonal_masks_8 !! (square`div`8 + square`mod`8) ) ) - (2 * reverse_bits binary_square) )
    possibilities_antidiagonal = ( (occupied .&. ( antidiagonal_masks_8 !! (square`div`8 + 7 - square`mod`8) ) ) - (2*binary_square) )
                                `xor` reverse_bits ( reverse_bits ( occupied .&. (antidiagonal_masks_8 !! (square`div`8 + 7 - square`mod`8) ) ) - (2 * reverse_bits binary_square) )
    in ( ( possibilities_diagonal .&. (diagonal_masks_8 !! (square`div`8 + square`mod`8) ) ) .|. ( possibilities_antidiagonal .&. (antidiagonal_masks_8 !! (square`div`8 + 7 - square`mod`8) ) ) )

-- The two below loop constructs are directly required by possibleWB function
-- These two are the loops that occur in the Java implementation done by Logic Chess Crazy
-- The two loop constructs can prove to be a standard for converting imperation code to functional code.
-- This is basically converting imperative code directly to functional code.
-- The function possibleWB has become so complicated because there was a need to maintain 'states'.
-- Though I have done this crudely by passing these variables (whose state needs to be maintained) as arguments to functions ..
-- a better alternative, I think, would be monads, which I don't understand as of now.
loop_j :: Word64 -> ByteString -> Word64 -> Int -> ByteString
loop_j 0 list possibility i_location = list
loop_j j list possibility i_location = let
    index = countTrailingZeros j
    list_mod = list `C.append` (C.pack $ show $ i_location`div`8) `C.append` (C.pack $ show $ i_location`mod`8)
                `C.append` (C.pack $ show $ index`div`8) `C.append` (C.pack $ show $ index`mod`8)
    possibility_mod = possibility .&. (complement j)
    j_mod = possibility_mod .&. complement (possibility_mod-1)
    in loop_j j_mod list_mod possibility_mod i_location

loop_i :: Word64 -> Word64 -> ByteString -> Word64 -> Word64 -> Word64 -> ByteString
loop_i 0 possibility list wb not_white_pieces occupied = list
loop_i i possibility list wb not_white_pieces occupied = let
    i_location = countTrailingZeros i
    possibility_mod = d_and_antid_moves i_location occupied .&. not_white_pieces
    j = possibility_mod .&. complement (possibility_mod-1)
    list_mod = loop_j j list possibility_mod i_location
    wb_mod = wb .&. complement i
    i_mod = wb_mod .&. complement (wb_mod-1)
    in loop_i i_mod possibility_mod list_mod wb_mod not_white_pieces occupied

possibleWB :: Word64 -> Word64 -> Word64 -> ByteString
possibleWB occupied wb not_white_pieces = let
    i = wb .&. complement (wb-1)
    dummy = 0
    list = loop_i i dummy "" wb not_white_pieces occupied
    in list
