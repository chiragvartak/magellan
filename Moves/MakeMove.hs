{-# LANGUAGE OverloadedStrings #-}

module Moves.MakeMove
    ( make_move
    , make_move_castle
    , make_move_ep
    ) where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt, isDigit, isUpper)

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- Takes a bitboard and a move and modifies the bitboard according to that move
make_move :: Word64 -> ByteString -> Char -> Word64
make_move board move piece_type
    | (isDigit $ C.index move 3) = let
            start = (digitToInt (C.index move 0))*8 + digitToInt (C.index move 1)
            end = (digitToInt (C.index move 2))*8 + digitToInt (C.index move 3)
            board_mod = if( (shiftR board start) .&. 1 == 1 )
                then
                    let
                    board_1 = board .&. complement (shiftL (1::Word64) start)
                    board_2 = board_1 .|. (shiftL (1::Word64) end)
                    in board_2
                else
                    board .&. complement (shiftL (1::Word64) end)
            in board_mod
    | (C.index move 3 == 'P') = let     -- Pawn Promotion
            start = if(isUpper $ C.index move 2)
                then countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 0))) .&. (rank_masks_8 !! 1) )
                else countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 0))) .&. (rank_masks_8 !! 6) )
            end = if(isUpper $ C.index move 2)
                then countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 1))) .&. (rank_masks_8 !! 0) )
                else countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 1))) .&. (rank_masks_8 !! 7) )
            board_mod = if(piece_type == C.index move 2)
                then board .|. shiftL (1::Word64) end
                else
                    let
                    board_1 = board .&. complement (shiftL (1::Word64) start)
                    board_2 = board .&. complement (shiftL (1::Word64) end)
                    in board_2
            in board_mod
    | (C.index move 3 == 'E') = let     -- En passant
            start = if(C.index move 2 == 'W')
                then countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 0))) .&. (rank_masks_8 !! 3) )
                else countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 0))) .&. (rank_masks_8 !! 4) )
            end = if(C.index move 2 == 'W')
                then countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 1))) .&. (rank_masks_8 !! 2) )
                else countTrailingZeros ( (file_masks_8 !! (digitToInt (C.index move 1))) .&. (rank_masks_8 !! 5) )
            board_mod = if(C.index move 2 == 'W')
                then board .&. complement ( (file_masks_8 !! (digitToInt (C.index move 1))) .&. (rank_masks_8 !! 3) )
                else board .&. complement ( (file_masks_8 !! (digitToInt (C.index move 1))) .&. (rank_masks_8 !! 4) )
            board_mod_2 = if( (shiftR board_mod start) .&. 1 == 1 )
                then
                    let
                    board_1 = board_mod .&. complement ( shiftL (1::Word64) start )
                    board_2 = board_1 .|. shiftL (1::Word64) end
                    in board_2
                else board_mod
            in board_mod_2
    | otherwise = 0

make_move_castle :: Word64 -> Word64 -> ByteString -> Char -> Word64
make_move_castle rook_board king_board move piece_type =
    let
    start = (digitToInt (C.index move 0))*8 + digitToInt (C.index move 1)
    return_rook_board = if ( (((shiftR king_board start) .&. 1) == 1)         -- Regular Move
                            && ( ( move == "0402" )
                            || ( move == "0406" )
                            || ( move == "7472" )
                            || ( move == "7476" ) )
                        )
        then if(piece_type == 'R')
            then
                case move of
                    "7472" ->
                        let
                        rook_board_1 = rook_board .&. complement ( shiftL (1::Word64) (castle_rooks!!1) )
                        rook_board_2 = rook_board_1 .|. ( shiftL (1::Word64) (castle_rooks!!1 + 3) )
                        in rook_board_2
                    "7476" ->
                        let
                        rook_board_1 = rook_board .&. complement ( shiftL (1::Word64) (castle_rooks!!0) )
                        rook_board_2 = rook_board_1 .|. ( shiftL (1::Word64) (castle_rooks!!0 - 2) )
                        in rook_board_2
                    xs -> rook_board
            else
                case move of
                    "0402" ->
                        let
                        rook_board_1 = rook_board .&. complement ( shiftL (1::Word64) (castle_rooks!!3) )
                        rook_board_2 = rook_board_1 .|. ( shiftL (1::Word64) (castle_rooks!!3 + 3) )
                        in rook_board_2
                    "0406" ->
                        let
                        rook_board_1 = rook_board .&. complement ( shiftL (1::Word64) (castle_rooks!!2) )
                        rook_board_2 = rook_board_1 .|. ( shiftL (1::Word64) (castle_rooks!!2 - 2) )
                        in rook_board_2
                    xs -> rook_board
        else
            rook_board
    in return_rook_board


make_move_ep :: Word64 -> ByteString -> Word64
make_move_ep board move =
    let
    start = (digitToInt (C.index move 0))*8 + digitToInt (C.index move 1)
    return_board = if( ( isDigit $ C.index move 3 )
                    && ( ( digitToInt (C.index move 0) - digitToInt (C.index move 2) ) `elem` [-2,2] )
                    && ( ( ( shiftR board start ) .&. 1 ) == 1 )
                    )
        then file_masks_8 !! ( digitToInt $ C.index move 1 )
        else 0
    in return_board
