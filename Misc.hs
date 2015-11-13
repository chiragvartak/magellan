module Misc where

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Foreign.Storable
import Data.Bits
import qualified Data.Word

-- Gives the binary representation of a number
-- Use only on positive numbers
binary_repr :: (Integral a, Show a, Storable a) => a -> String
binary_repr num =
  let raw_string = showIntAtBase 2 intToDigit num ""
      number_padded_zeros = (sizeOf num)*8 - length raw_string
  in replicate number_padded_zeros '0' ++ raw_string

reverse_bits :: Data.Word.Word64 -> Data.Word.Word64
reverse_bits v = let
	v0 = v
	v1 = ( (shiftR v0 1) .&. 0x5555555555555555 ) .|. ( shiftL (v0 .&. 0x5555555555555555) 1 )
	v2 = ( (shiftR v1 2) .&. 0x3333333333333333 ) .|. ( shiftL (v1 .&. 0x3333333333333333) 2 )
	v3 = ( (shiftR v2 4) .&. 0x0F0F0F0F0F0F0F0F ) .|. ( shiftL (v2 .&. 0x0F0F0F0F0F0F0F0F) 4 )
	v4 = ( (shiftR v3 8) .&. 0x00FF00FF00FF00FF ) .|. ( shiftL (v3 .&. 0x00FF00FF00FF00FF) 8 )
	v5 = ((shiftR v4 16) .&. 0x0000FFFF0000FFFF ) .|. ( shiftL (v4 .&. 0x0000FFFF0000FFFF) 16)
	v6 = ((shiftR v5 32) .&. 0x00000000FFFFFFFF ) .|. ( shiftL (v5 .&. 0x00000000FFFFFFFF) 32)
	in v6
