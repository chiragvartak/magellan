-- This GHC extension makes the string literals polymorphic.
-- This makes it easy to use ByteStrings. 
{-# LANGUAGE OverloadedStrings #-}

module Temp where

-- Just import the Data.ByteString.Char8 interface.
-- No need to import Data.ByteString as well. Char8 has it all.
import qualified Data.ByteString.Char8 as C

type ByteString = C.ByteString

str = 	['A', 'B', 'C'
		,'P', 'Q', 'R']

data Person = Person {firstName :: String  
                     ,lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   

-- Slice the ByteString from index m to n, n exclusive
slice :: ByteString -> Int -> Int -> ByteString
slice str m n = C.drop m (C.take n str)

string1 :: ByteString
string1 = "Darth Vader"

string2 :: ByteString
string2 = "Obi-Wan Kenobi"

string3 :: ByteString
string3 = "Darth Vader"

bool1 = string3 == "Darth Vader"