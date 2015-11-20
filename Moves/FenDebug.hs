-- This module defines functions that convert a FEN string to a Position
module Moves.FenDebug(
  import_fen
  )where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import Data.Char (digitToInt, ord)
import qualified Data.Text as T

type Word64 = Data.Word.Word64

-- Just a few FEN Strings for perspective
fen1 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
fen2 = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"

-- Just a dummy Position for modifying later
pos_dummy = Position 0 0 0 0 0 0 0 0 0 0 0 0 0 False False False False

import_fen :: String -> Position
import_fen fenstr =
  let
  --String until the first space
  str1 = T.unpack $ fst $ T.breakOn (T.pack " ") (T.pack fenstr)
  -- String after the first space
  str2 = T.unpack $ snd $ T.breakOn (T.pack " ") (T.pack fenstr)

  -- Repeating the numbers
  str3 = repeat_numbers str1

  -- Modifying the Position to place pieces on the board
  pairs_list = zip str3 [(0::Int)..]
  pos1 = foldl modify_bitboards pos_dummy pairs_list

  -- Modifying the Position to initialize castling booleans and en passant bitboard
  pos2 = foldl modify_ec pos1 str2

  -- Returning the final position
  in pos2

-- Use this function on str1
modify_bitboards :: Position -> (Char,Int) -> Position
modify_bitboards pos (ch, index)
  | (ch == 'P') = Position ((wp pos) .|. shiftL (1::Word64) index) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'N') = Position (wp pos) ((wn pos) .|. shiftL (1::Word64) index) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'B') = Position (wp pos) (wn pos) ((wb pos) .|. shiftL (1::Word64) index) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'R') = Position (wp pos) (wn pos) (wb pos) ((wr pos) .|. shiftL (1::Word64) index) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'Q') = Position (wp pos) (wn pos) (wb pos) (wr pos) ((wq pos) .|. shiftL (1::Word64) index) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'K') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) ((wk pos) .|. shiftL (1::Word64) index)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'p') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              ((bp pos) .|. shiftL (1::Word64) index) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'n') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) ((bn pos) .|. shiftL (1::Word64) index) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'b') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) ((bb pos) .|. shiftL (1::Word64) index) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'r') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) ((br pos) .|. shiftL (1::Word64) index) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'q') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) ((bq pos) .|. shiftL (1::Word64) index) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'k') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) ((bk pos) .|. shiftL (1::Word64) index)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | otherwise = pos

repeat_numbers :: String -> String
repeat_numbers fenstrpart = merge_strings [repeat_them ch | ch <- fenstrpart]
  where
    repeat_them :: Char -> String
    repeat_them ch
      | (ch == '2') = "22"
      | (ch == '3') = "333"
      | (ch == '4') = "4444"
      | (ch == '5') = "55555"
      | (ch == '6') = "666666"
      | (ch == '7') = "7777777"
      | (ch == '8') = "88888888"
      | (ch == '/') = ""
      | otherwise = [ch]

merge_strings :: [String] -> String
merge_strings xxs = [x | xs <- xxs, x <- xs]

modify_ec :: Position -> Char -> Position
modify_ec pos ch
  | (ch == 'K') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) True (cwq pos) (cbk pos) (cbq pos)
  | (ch == 'Q') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) True (cbk pos) (cbq pos)
  | (ch == 'k') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) True (cbq pos)
  | (ch == 'q') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (ep pos) (cwk pos) (cwq pos) (cbk pos) True
  | (ch == '-') = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              0 (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | (ch `elem` ['a'..'h']) = Position (wp pos) (wn pos) (wb pos) (wr pos) (wq pos) (wk pos)
                              (bp pos) (bn pos) (bb pos) (br pos) (bq pos) (bk pos)
                              (file_masks_8 !! (ord ch - ord 'a')) (cwk pos) (cwq pos) (cbk pos) (cbq pos)
  | otherwise = pos
