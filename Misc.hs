module Misc where

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Foreign.Storable

-- Gives the binary representation of a number
-- Use only on positive numbers
binary_repr :: (Integral a, Show a, Storable a) => a -> String
binary_repr num =
  let raw_string = showIntAtBase 2 intToDigit num ""
      number_padded_zeros = (sizeOf num)*8 - length raw_string
  in replicate number_padded_zeros '0' ++ raw_string

binary_repr' :: (Integral a) => a -> String
binary_repr' val = 