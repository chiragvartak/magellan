-- This GHC extension makes the string literals polymorphic.
-- This makes it easy to use ByteStrings. 
{-# LANGUAGE OverloadedStrings #-}

module Temp where

-- Just import the Data.ByteString.Char8 interface.
-- No need to import Data.ByteString as well. Char8 has it all.
import qualified Data.ByteString.Char8 as C
import Data.Char (intToDigit)

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

bool1 = string1 == "Darth Vader"

-- Addition and subtraction of Chars
char_add :: Char -> Int -> Char
char_add ch n = ( toEnum (fromEnum ch + n) ) :: Char

move_to_algebra :: ByteString -> ByteString
move_to_algebra move = 
    C.pack  [ char_add (C.index move 1) 49
            , intToDigit ( fromEnum '8' - fromEnum (C.index move 0) )
            , char_add (C.index move 3) 49
            , intToDigit ( fromEnum '8' - fromEnum (C.index move 2) )
            ]

{-
public static String moveToAlgebra(String move)
    {
        String moveString="";
        moveString+=""+(char)(move.charAt(1)+49);
        moveString+=""+('8'-move.charAt(0));
        moveString+=""+(char)(move.charAt(3)+49);
        moveString+=""+('8'-move.charAt(2));
        return moveString;
    }
-}