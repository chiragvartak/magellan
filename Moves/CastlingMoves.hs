{-# LANGUAGE OverloadedStrings #-}

module Moves.CastlingMoves
    ( possibleCW
    , possibleCB
    ) where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

possibleCW :: Word64 -> Bool -> Bool -> Word64 -> ByteString
possibleCW wr cwk cwq occupied = let
	list1 = if (cwk
				&& ( ( shiftL (1::Word64) (castle_rooks!!0) .&. wr ) /= 0 ) 
				&& ( ( occupied .&. ( (shiftL (1::Word64) 61) .|. (shiftL (1::Word64) 62) ) ) == 0 )
				)
			then "7476"::ByteString
			else ""
	list2 = if (cwq
				&& ( ( shiftL (1::Word64) (castle_rooks!!1) .&. wr ) /= 0 ) 
				&& ( ( occupied .&. ( (shiftL (1::Word64) 57) .|. (shiftL (1::Word64) 58) .|. (shiftL (1::Word64) 59) ) ) == 0 )
				)
			then "7472"::ByteString
			else ""
	list = list1 `C.append` list2
	in list

possibleCB :: Word64 -> Bool -> Bool -> Word64 -> ByteString
possibleCB br cbk cbq occupied = let
	list1 = if (cbk
				&& ( ( shiftL (1::Word64) (castle_rooks!!2) .&. br ) /= 0 ) 
				&& ( ( occupied .&. ( (shiftL (1::Word64) 5) .|. (shiftL (1::Word64) 6) ) ) == 0 )
				)
			then "0406"::ByteString
			else ""
	list2 = if (cbq
				&& ( ( shiftL (1::Word64) (castle_rooks!!3) .&. br ) /= 0 ) 
				&& ( ( occupied .&. ( (shiftL (1::Word64) 1) .|. (shiftL (1::Word64) 2) .|. (shiftL (1::Word64) 3) ) ) == 0 )
				)
			then "0402"::ByteString
			else ""
	list = list1 `C.append` list2
	in list
