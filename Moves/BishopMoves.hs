{-# LANGUAGE OverloadedStrings #-}

module Moves.BishopMoves
    ( possibleWB
    ) where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

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
