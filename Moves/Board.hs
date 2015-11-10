{-# LANGUAGE OverloadedStrings #-}

module Moves.Board where

import Moves.HelperFunctions
import Moves.PawnMoves
import Moves.BishopMoves
import Moves.RookMoves
import Moves.QueenMoves
import Moves.KnightMoves
import Moves.KingMoves
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- This function gives all the possible moves of white. This is the one you should call.
-- If you want just one, say, pawn moves, comment the rest in this function and debug. 
possible_moves_w :: Position -> ByteString
possible_moves_w pos = let
    not_white_pieces = complement (wp pos .|. wn pos .|. wb pos .|. wr pos .|. wq pos .|. wk pos .|. bk pos)
    black_pieces = bp pos .|. bn pos .|. bb pos .|. br pos .|. bq pos
    empty = complement ((wp pos) .|. (wn pos) .|. (wb pos) .|. (wr pos) .|. (wq pos) .|. (wk pos) .|. (bp pos) .|. (bn pos)
        .|. (bb pos) .|. (br pos) .|. (bq pos) .|. (bk pos))
    occupied = complement empty
    list = possibleWP2 (history pos) (wp pos) (bp pos) not_white_pieces black_pieces empty
            `C.append` possibleWN occupied (wn pos) not_white_pieces
            `C.append` possibleWB occupied (wb pos) not_white_pieces
            `C.append` possibleWR occupied (wr pos) not_white_pieces
            `C.append` possibleWQ occupied (wq pos) not_white_pieces
            `C.append` possibleWK occupied (wk pos) not_white_pieces
    in list
