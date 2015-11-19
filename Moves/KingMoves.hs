{-# LANGUAGE OverloadedStrings #-}

module Moves.KingMoves
	( unsafe_for_black
	, unsafe_for_white
	, possibleK
	)where

import Moves.HelperFunctions
import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

-- A function that gives a bitboard showing all the places where the white pieces are attacking.
-- Thus this is a bitboard where the black king can't move to.
unsafe_for_black :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
unsafe_for_black wp wn wb wr wq wk bp bn bb br bq bk = let
	occupied = wp .|. wn .|. wb .|. wr .|. wq .|. wk .|. bp .|. bn .|. bb .|. br .|. bq .|. bk
	
	-- Pawn
	unsafe_1 = (shiftR wp 7) .&. (complement file_a)
	unsafe_2 = unsafe_1 .|. ( (shiftR wp 9) .&. (complement file_h) )

	-- Knight
	i_knight = wn .&. complement (wn-1)
	dummy = 0
	unsafe_3 = loop_i_knight i_knight dummy unsafe_2 wn

	-- Bishop/Queen
	qb = wq .|. wb
	i_qb = qb .&. complement (qb-1)
	unsafe_4 = loop_i_qb i_qb dummy unsafe_3 qb occupied

	-- Rook/Queen
	qr = wq .|. wr
	i_qr = qr .&. complement (qr-1)
	unsafe_5 = loop_i_qr i_qr dummy unsafe_4 qr occupied

	-- King
	i_locn_king = countTrailingZeros wk
	possibility_king_1 = if(i_locn_king > 9)
		then shiftL king_span (i_locn_king-9)
		else shiftR king_span (9-i_locn_king)
	possibility_king_2 = if(i_locn_king`mod`8 < 4)
		then possibility_king_1 .&. complement file_gh
		else possibility_king_1 .&. complement file_ab
	unsafe_6 = unsafe_5 .|. possibility_king_2

	in unsafe_6

unsafe_for_white :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
unsafe_for_white wp wn wb wr wq wk bp bn bb br bq bk = let
	occupied = wp .|. wn .|. wb .|. wr .|. wq .|. wk .|. bp .|. bn .|. bb .|. br .|. bq .|. bk
	
	-- Pawn
	unsafe_1 = (shiftL bp 7) .&. (complement file_h)
	unsafe_2 = unsafe_1 .|. ( (shiftL bp 9) .&. (complement file_a) )

	-- Knight
	i_knight = bn .&. complement (bn-1)
	dummy = 0
	unsafe_3 = loop_i_knight i_knight dummy unsafe_2 bn

	-- Bishop/Queen
	qb = bq .|. bb
	i_qb = qb .&. complement (qb-1)
	unsafe_4 = loop_i_qb i_qb dummy unsafe_3 qb occupied

	-- Rook/Queen
	qr = bq .|. br
	i_qr = qr .&. complement (qr-1)
	unsafe_5 = loop_i_qr i_qr dummy unsafe_4 qr occupied

	-- King
	i_locn_king = countTrailingZeros bk
	possibility_king_1 = if(i_locn_king > 9)
		then shiftL king_span (i_locn_king-9)
		else shiftR king_span (9-i_locn_king)
	possibility_king_2 = if(i_locn_king`mod`8 < 4)
		then possibility_king_1 .&. complement file_gh
		else possibility_king_1 .&. complement file_ab
	unsafe_6 = unsafe_5 .|. possibility_king_2

	in unsafe_6

possibleK :: Word64 -> Word64 -> Word64 -> ByteString
possibleK occupied k cannot_capture = let
	i_location = countTrailingZeros k
	possibility_1 = if(i_location > 9)
		then shiftL king_span (i_location-9)
		else shiftR king_span (9 - i_location)
	possibility_2 = if(i_location`mod`8 < 4)
		then possibility_1 .&. ( complement file_gh .&. cannot_capture )
		else possibility_1 .&. ( complement file_ab .&. cannot_capture )
	j = possibility_2 .&. complement (possibility_2-1)
	list = loop_j j "" possibility_2 i_location
	in list

loop_j :: Word64 -> ByteString -> Word64 -> Int -> ByteString
loop_j 0 list possibility i_location = list
loop_j j list possibility i_location = let
	index = countTrailingZeros j
	list_mod = list `C.append` (C.pack $ show $ i_location`div`8) `C.append` (C.pack $ show $ i_location`mod`8)
				`C.append` (C.pack $ show $ index`div`8) `C.append` (C.pack $ show $ index`mod`8) 
	possibility_mod = possibility .&. complement j
	j_mod = possibility_mod .&. complement (possibility_mod-1)
	in loop_j j_mod list_mod possibility_mod i_location

loop_i_qr :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
loop_i_qr 0 possibility unsafe qr occupied = unsafe
loop_i_qr i_qr possibility unsafe qr occupied = let
	i_location = countTrailingZeros i_qr
	possibility_1 = h_and_v_moves i_location occupied
	unsafe_mod = unsafe .|. possibility_1
	qr_mod = qr .&. complement i_qr
	i_mod = qr_mod .&. complement (qr_mod-1)
	in loop_i_qr i_mod possibility_1 unsafe_mod qr_mod occupied

loop_i_qb :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
loop_i_qb 0 possibility unsafe qb occupied = unsafe
loop_i_qb i_qb possibility unsafe qb occupied = let
	i_location = countTrailingZeros i_qb
	possibility_1 = d_and_antid_moves i_location occupied
	unsafe_mod = unsafe .|. possibility_1
	qb_mod = qb .&. complement i_qb
	i_mod = qb_mod .&. complement (qb_mod-1)
	in loop_i_qb i_mod possibility_1 unsafe_mod qb_mod occupied

loop_i_knight :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
loop_i_knight 0 possibility unsafe wn = unsafe
loop_i_knight i_knight possibility unsafe wn = let
	i_location = countTrailingZeros i_knight
	possibility1 = if(i_location>18)
		then shiftL knight_span (i_location - 18)
		else shiftR knight_span (18 - i_location)
	possibility2 = if(i_location`mod`8 < 4)
		then possibility1 .&. (complement file_gh)
		else possibility1 .&. (complement file_ab)
	unsafe_mod = unsafe .|. possibility2
	wn_mod = wn .&. complement i_knight
	i_mod = wn_mod .&. complement (wn_mod-1)
	in loop_i_knight i_mod possibility2 unsafe_mod wn_mod
