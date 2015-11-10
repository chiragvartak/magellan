-- This GHC extension makes the string literals polymorphic.
-- This makes it easy to use ByteStrings. 
{-# LANGUAGE OverloadedStrings #-}

module Moves.HelperFunctions
	( reverse_bits
	, h_and_v_moves
	, d_and_antid_moves
    , file_a, file_h, file_ab, file_gh, rank_8, rank_5, rank_4, rank_1, centre, extended_centre, king_side, queen_side, king_span, knight_span
    , rank_masks_8, file_masks_8, diagonal_masks_8, antidiagonal_masks_8
    , Position(..)
	) where

import qualified Data.Word
import Data.Bits
-- Just import the Data.ByteString.Char8 interface.
-- No need to import Data.ByteString as well. Char8 has it all.
-- ByteStrings are more efficient than Strings in Haskell, wrt both space and time complexity
-- Hence we will be using ByteStrings in place of Strings.
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- This construct contains all the information required to represent a state completely.
-- Does it?
data Position = Position { history :: ByteString, wp :: Word64, wn :: Word64, wb :: Word64, wr :: Word64, wq :: Word64, wk :: Word64
                        , bp :: Word64, bn :: Word64, bb :: Word64, br :: Word64, bq :: Word64, bk :: Word64
                        } deriving (Show)

-- Things required for the move generation functions
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
king_span = 460039::Word64;
knight_span = 43234889994::Word64;
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
