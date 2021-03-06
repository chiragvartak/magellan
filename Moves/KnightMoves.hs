{-# LANGUAGE OverloadedStrings #-}

module Moves.KnightMoves
    ( possibleN
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
loop_i 0 possibility list wn cannot_capture occupied = list
loop_i i possibility list wn cannot_capture occupied = let
    i_location = countTrailingZeros i
    possibility_mod0 = if(i_location > 18)
        then shiftL knight_span (i_location-18)
        else shiftR knight_span (18-i_location)
    possibility_mod = if(i_location`mod`8 < 4)
        then possibility_mod0 .&. (complement file_gh) .&. cannot_capture
        else possibility_mod0 .&. (complement file_ab) .&. cannot_capture
    j = possibility_mod .&. complement (possibility_mod-1)
    list_mod = loop_j j list possibility_mod i_location
    wn_mod = wn .&. complement i
    i_mod = wn_mod .&. complement (wn_mod-1)
    in loop_i i_mod possibility_mod list_mod wn_mod cannot_capture occupied

possibleN :: Word64 -> Word64 -> Word64 -> ByteString
possibleN occupied n cannot_capture = let
    i = n .&. complement (n-1)
    dummy = 0
    list = loop_i i dummy "" n cannot_capture occupied
    in list
