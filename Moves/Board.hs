{-# LANGUAGE OverloadedStrings #-}

module Moves.Board where

import Moves.HelperFunctions
import Moves.PawnMoves
import Moves.BishopMoves
import Moves.RookMoves
import Moves.QueenMoves
import Moves.KnightMoves
import Moves.KingMoves
import Moves.CastlingMoves
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- This function gives all the possible moves of white. This is the one you should call.
-- If you want just one, say, pawn moves, comment the rest in this function and debug. 
possible_moves_w :: Position -> ByteString
possible_moves_w pos = let
    cannot_capture = complement (wp pos .|. wn pos .|. wb pos .|. wr pos .|. wq pos .|. wk pos .|. bk pos)
    can_capture = bp pos .|. bn pos .|. bb pos .|. br pos .|. bq pos
    occupied = (wp pos) .|. (wn pos) .|. (wb pos) .|. (wr pos) .|. (wq pos) .|. (wk pos) .|. (bp pos) .|. (bn pos)
                .|. (bb pos) .|. (br pos) .|. (bq pos) .|. (bk pos)
    empty = complement occupied
    list = possibleWP2 (history pos) (wp pos) (bp pos) (ep pos) cannot_capture can_capture empty
            `C.append` possibleN occupied (wn pos) cannot_capture
            `C.append` possibleB occupied (wb pos) cannot_capture
            `C.append` possibleR occupied (wr pos) cannot_capture
            `C.append` possibleQ occupied (wq pos) cannot_capture
            `C.append` possibleK occupied (wk pos) cannot_capture
            `C.append` possibleCW (wr pos) (cwk pos) (cwq pos) occupied
    in list

possible_moves_b :: Position -> ByteString
possible_moves_b pos = let
    cannot_capture = complement (bp pos .|. bn pos .|. bb pos .|. br pos .|. bq pos .|. bk pos .|. wk pos)
    can_capture = wp pos .|. wn pos .|. wb pos .|. wr pos .|. wq pos
    occupied = (wp pos) .|. (wn pos) .|. (wb pos) .|. (wr pos) .|. (wq pos) .|. (wk pos) .|. (bp pos) .|. (bn pos)
                .|. (bb pos) .|. (br pos) .|. (bq pos) .|. (bk pos)
    empty = complement occupied
    list = possibleBP2 (history pos) (wp pos) (bp pos) (ep pos) cannot_capture can_capture empty
            `C.append` possibleN occupied (bn pos) cannot_capture
            `C.append` possibleB occupied (bb pos) cannot_capture
            `C.append` possibleR occupied (br pos) cannot_capture
            `C.append` possibleQ occupied (bq pos) cannot_capture
            `C.append` possibleK occupied (bk pos) cannot_capture
            `C.append` possibleCB (br pos) (cbk pos) (cbq pos) occupied
    in list
