{-# LANGUAGE OverloadedStrings #-}

module Moves.RookMoves
    ( possibleWR
    ) where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

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
loop_i 0 possibility list wr not_white_pieces occupied = list
loop_i i possibility list wr not_white_pieces occupied = let
    i_location = countTrailingZeros i
    possibility_mod = h_and_v_moves i_location occupied .&. not_white_pieces
    j = possibility_mod .&. complement (possibility_mod-1)
    list_mod = loop_j j list possibility_mod i_location
    wr_mod = wr .&. complement i
    i_mod = wr_mod .&. complement (wr_mod-1)
    in loop_i i_mod possibility_mod list_mod wr_mod not_white_pieces occupied

possibleWR :: Word64 -> Word64 -> Word64 -> ByteString
possibleWR occupied wr not_white_pieces = let
    i = wr .&. complement (wr-1)
    dummy = 0
    list = loop_i i dummy "" wr not_white_pieces occupied
    in list
