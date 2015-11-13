{-# LANGUAGE OverloadedStrings #-}

module Moves.Perft
    ( perft
    ) where

import qualified Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C

type Word64 = Data.Word.Word64
type ByteString = C.ByteString

perft_max_depth = 1

perft :: Position -> Bool -> Int -> Int -> IO ()
perft pos white_to_move depth perft_move_counter = do
    if (depth < perft_max_depth)
    then do
        let
            moves = if( white_to_move )
                then possible_moves_w pos
                else possible_moves_b pos
        loop_i 0 pos white_to_move depth moves perft_move_counter
    else do
        print perft_move_counter
        return ()

-- Initially call with i=0
loop_i :: Int -> Position -> Bool -> Int -> ByteString -> Int -> IO ()
loop_i i pos white_to_move depth moves perft_move_counter = do
    if(i < C.length moves)
    then do
        let
            wpt = make_move (wp pos) (slice moves i i+4) 'P'
            wnt = make_move (wn pos) (slice moves i i+4) 'N'
            wbt = make_move (wb pos) (slice moves i i+4) 'B'
            wrt = make_move (wr pos) (slice moves i i+4) 'R'
            wqt = make_move (wq pos) (slice moves i i+4) 'Q'
            wkt = make_move (wk pos) (slice moves i i+4) 'K'
            bpt = make_move (bp pos) (slice moves i i+4) 'p'
            bnt = make_move (bn pos) (slice moves i i+4) 'n'
            bbt = make_move (bb pos) (slice moves i i+4) 'b'
            brt = make_move (br pos) (slice moves i i+4) 'r'
            bqt = make_move (bq pos) (slice moves i i+4) 'q'
            bkt = make_move (bk pos) (slice moves i i+4) 'k'
            ept = make_move_rp (wp pos .|. bp pos) (slice moves i i+4)

            wrt2 = make_move_castle wrt (wk pos .|. bk pos) (slice moves i i+4) 'R'
            brt2 = make_move_castle brt (wk pos .|. bk pos) (slice moves i i+4) 'r'

            (cwkt, cwqt, cbkt, cbqt) = (cwk, cwq, cbk, cbq)

            start = (digitToInt $ C.index moves (i))*8 + (digitToInt $ C.index moves (i+1))
            (cwkt2, cwqt2, cbkt2, cbqt2) = if( isDigit $ C.index moves (i+3) )      --regular move
                then
                    if( (shiftL (1::Word64) start) .&. (wk pos) /= 0 )
                        then (False, False, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (bk pos) /= 0 )
                        then (cwkt, cwqt, False, False)
                    else if ( (shiftL (1::Word64) start) .&. (wr pos) .&. ( shiftL (1::Word64) 63 ) \= 0 )
                        then (False, cwqt, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (wr pos) .&. ( shiftL (1::Word64) 56 ) \= 0 )
                        then (cwkt, False, cbkt, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (br pos) .&. ( shiftL (1::Word64) 7 ) \= 0 )
                        then (cwkt, cwqt, False, cbqt)
                    else if ( (shiftL (1::Word64) start) .&. (br pos) .&. (1::Word64) \= 0 )
                        then (cwkt, cwqt, cbkt, False)
                else (cwkt, cwqt, cbkt, cbqt)

        if( ( ( wkt .&. unsafe_for_white wpt wnt wbt wrt wqt wkt bpt bnt bbt brt bqt bkt ) == 0 && white_to_move ) || 
            ( ( bkt .&. unsafe_for_black wpt wnt wbt wrt wqt wkt bpt bnt bbt brt bqt bkt ) == 0 && (not white_to_move) )
        )
            then do
                let
                    perft_move_counter_mod = if (depth+1 == perft_max_depth)
                        then (perft_move_counter+1)
                        else perft_move_counter
                perft (Position wpt wnt wbt wrt wqt wkt bpt bnt bbt brt bqt bkt ept cwkt2 cwqt2 cbkt2 cbqt2) (not white_to_move) (depth+1) perft_move_counter_mod
            else 
                return ()

        -- Looping
        loop_i (i+1) pos white_to_move depth moves perft_move_counter_mod

    else return ()

{-

public static void perft(long WP,long WN,long WB,long WR,long WQ,long WK,long BP,long BN,long BB,long BR,long BQ,long BK,long EP,boolean CWK,boolean CWQ,boolean CBK,boolean CBQ,boolean WhiteToMove,int depth)
    {
        if (depth<perftMaxDepth) {
            String moves;
            if (WhiteToMove) {
                moves=Moves.possibleMovesW(WP,WN,WB,WR,WQ,WK,BP,BN,BB,BR,BQ,BK,EP,CWK,CWQ,CBK,CBQ);
            } else {
                moves=Moves.possibleMovesB(WP,WN,WB,WR,WQ,WK,BP,BN,BB,BR,BQ,BK,EP,CWK,CWQ,CBK,CBQ);
            }
            for (int i=0;i<moves.length();i+=4) {
                long WPt=Moves.makeMove(WP, moves.substring(i,i+4), 'P'), WNt=Moves.makeMove(WN, moves.substring(i,i+4), 'N'),
                        WBt=Moves.makeMove(WB, moves.substring(i,i+4), 'B'), WRt=Moves.makeMove(WR, moves.substring(i,i+4), 'R'),
                        WQt=Moves.makeMove(WQ, moves.substring(i,i+4), 'Q'), WKt=Moves.makeMove(WK, moves.substring(i,i+4), 'K'),
                        BPt=Moves.makeMove(BP, moves.substring(i,i+4), 'p'), BNt=Moves.makeMove(BN, moves.substring(i,i+4), 'n'),
                        BBt=Moves.makeMove(BB, moves.substring(i,i+4), 'b'), BRt=Moves.makeMove(BR, moves.substring(i,i+4), 'r'),
                        BQt=Moves.makeMove(BQ, moves.substring(i,i+4), 'q'), BKt=Moves.makeMove(BK, moves.substring(i,i+4), 'k'),
                        EPt=Moves.makeMoveEP(WP|BP,moves.substring(i,i+4));
                WRt=Moves.makeMoveCastle(WRt, WK|BK, moves.substring(i,i+4), 'R');
                BRt=Moves.makeMoveCastle(BRt, WK|BK, moves.substring(i,i+4), 'r');
                boolean CWKt=CWK,CWQt=CWQ,CBKt=CBK,CBQt=CBQ;
                if (Character.isDigit(moves.charAt(i+3))) {//'regular' move
                    int start=(Character.getNumericValue(moves.charAt(i))*8)+(Character.getNumericValue(moves.charAt(i+1)));
                    if (((1L<<start)&WK)!=0) {CWKt=false; CWQt=false;}
                    else if (((1L<<start)&BK)!=0) {CBKt=false; CBQt=false;}
                    else if (((1L<<start)&WR&(1L<<63))!=0) {CWKt=false;}
                    else if (((1L<<start)&WR&(1L<<56))!=0) {CWQt=false;}
                    else if (((1L<<start)&BR&(1L<<7))!=0) {CBKt=false;}
                    else if (((1L<<start)&BR&1L)!=0) {CBQt=false;}
                }
                if (((WKt&Moves.unsafeForWhite(WPt,WNt,WBt,WRt,WQt,WKt,BPt,BNt,BBt,BRt,BQt,BKt))==0 && WhiteToMove) ||
                        ((BKt&Moves.unsafeForBlack(WPt,WNt,WBt,WRt,WQt,WKt,BPt,BNt,BBt,BRt,BQt,BKt))==0 && !WhiteToMove)) {
                    if (depth+1==perftMaxDepth) {perftMoveCounter++;}
                    perft(WPt,WNt,WBt,WRt,WQt,WKt,BPt,BNt,BBt,BRt,BQt,BKt,EPt,CWKt,CWQt,CBKt,CBQt,!WhiteToMove,depth+1);
                }
            }
        }
    }

-}

-- Slice the ByteString from index m to n, n exclusive
slice :: ByteString -> Int -> Int -> ByteString
slice str m n = C.drop m (C.take n str)