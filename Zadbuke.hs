module Zadbuke where

import Data.Bits
import System.Random as Rand
import Data.Array.Unboxed as UArray
import Text.Printf
import Data.Word

type BB = Word64


-- converts a linear index into a rank-file pair
linToRF :: Int -> (Int, Int)
linToRF sq = quotRem sq 8

-- converts a rank-file pair to a linear index
rfToLin :: (Int, Int) -> Int
rfToLin (rank, file) = rank*8 + file

-- print a bitboard in a 8x8 view
print64 :: BB -> IO ()
print64 b = let l64 = replicate 64 b
                shifts = 1:(take 63 (map (shift 1) [1..])) :: [BB]
                inter = zipWith (.&.) shifts l64
                result = (map fromIntegral) $ zipWith shift inter (map (*(-1))[0..63]) :: [Int]
            in printLikeABoardIterate 0 result


-- helper function for above
printLikeABoardIterate :: Int -> [Int] -> IO ()
printLikeABoardIterate 64 _ = putStr "\n"
printLikeABoardIterate i xs = do let realIndex = \a -> 64 - 8*( truncate ((fromIntegral (a+8))/8) ) + (a `rem` 8) :: Int
                                 if i `rem` 8 == 0
                                     then putStr $ "\n"
                                     else return ()
                                 let x = (xs !! (realIndex i))
                                 putStr $ ( show x ) ++ " "
                                 printLikeABoardIterate (i+1) xs

-- Reverses the bits of a Word64
-- Using this a few times is alright but using it continually will create problems.
-- This shouldn't be used in generating moves because we typically want our move generation to be quick.
-- In move generation we just want to have fast, bitwise operations.
-- Any reverse will typically take O(n), where n will be typically as large as 64.
reverse' :: BB -> BB
reverse' brd = createBB [(63-i)|i<-[0..63],(testBit brd i)] 

-- Takes the a list of the bit-numbers and sets them to 1, thereby generating a bitboard
createBB :: [Int] -> BB
--createBB xs = (sum (zipWith (^) (repeat 2) xs))
createBB xs = foldr (flip$setBit) 0 xs

genRookhMask :: Int -> BB
genRookhMask x = clearBit (createBB ([t*8+y|y<-[0..7]])) x
				where
					t = (div x 8)

genRookvMask :: Int -> BB
genRookvMask x = clearBit (createBB ([u+(8*y)|y<-[0..7]])) x
				where
					u = (mod x 8)					
					
genRookvMoves :: Int -> BB -> BB
genRookvMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - 2*(reverse' sli)))) .&. mask
						where
							sli = (createBB [pos])
							mask = (genRookvMask pos)
							
genRookhMoves :: Int -> BB -> BB
genRookhMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - 2*(reverse' sli)))) .&. mask
						where
							sli = (createBB [pos])
							mask = (genRookhMask pos)

genRookMoves :: Int -> BB -> BB	-> BB
genRookMoves pos occ self = ((genRookvMoves pos occ) .|. 	(genRookhMoves pos occ)) .&. (complement self)


ldiag :: (Int,Int) -> [(Int,Int)]
ldiag (a,b) = [(a+i,b+i)|i<-[0..(7-(max a b))]] ++ [(a-i,b-i)|i<-[0..(min a b)]]

fdiag :: (Int,Int) -> [(Int,Int)]
fdiag (a,b) = [(a-i,b+i)|i<-[0..(min (a) (7-b))]] ++ [(a+i,b-i)|i<-[0..(min (b) (7-a))]]
		
genBishoplMask :: Int -> BB		
genBishoplMask x = clearBit (createBB (map (rfToLin) (ldiag (linToRF x)))) x

genBishopfMask :: Int -> BB		
genBishopfMask x = clearBit (createBB (map (rfToLin) (fdiag (linToRF x)))) x

genBishopfMoves :: Int -> BB -> BB
genBishopfMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - (reverse'(2*sli))))) .&. mask
						where
							sli = (createBB [pos])
							mask = (genBishopfMask pos)	
							
genBishoplMoves :: Int -> BB -> BB
genBishoplMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - (reverse'(2*sli))))) .&. mask
						where
							sli = (createBB [pos])
							mask = (genBishoplMask pos)		
							
genBishopMoves :: Int -> BB -> BB -> BB	
genBishopMoves pos occ self = ((genBishoplMoves pos occ) .|. 	(genBishopfMoves pos occ))	.&. (complement self)

genQueenMoves :: Int -> BB -> BB -> BB	
genQueenMoves pos occ self = ((genBishoplMoves pos occ) .|. 	(genBishopfMoves pos occ) .|. (genRookvMoves pos occ) .|. 	(genRookhMoves pos occ)) .&. (complement self)


genKnightMoves :: Int -> BB -> BB
genKnightMoves pos self = (createBB (filter (>(-1)) (filter (<64) [pos+x|x<-[6,10,15,17,(-6),(-10),(-15),(-17)]])) .&. (knightRange pos)) .&. (complement self)

knightRange :: Int -> BB
knightRange pos = createBB [8*x+y|x<-[hll..hul],y<-[vll..vul]]
				where
					(posx, posy) = (linToRF pos)
					hll = max 0 (posx-2)
					hul = min 7 (posx+2)
					vll = max 0 (posy-2)
					vul = min 7 (posy+2)
					

genKingMoves :: Int -> BB -> BB
genKingMoves pos self = (createBB (filter (>(-1)) (filter (<64) [pos+x|x<-[1,7,8,9,(-1),(-7),(-8),(-9)]])) .&. (kingRange pos)) .&. (complement self)

kingRange :: Int -> BB
kingRange pos = createBB [8*x+y|x<-[hll..hul],y<-[vll..vul]]
				where
					(posx, posy) = (linToRF pos)
					hll = max 0 (posx-1)
					hul = min 7 (posx+1)
					vll = max 0 (posy-1)
					vul = min 7 (posy+1)




--genKnightMoves :: Int -> BB
--genKnightMoves pos = createBB (filter (>(-1)) (filter (<64) [pos+x|x<-[6,10,15,17,(-6),(-10),(-15),(-17)]])) .&. (kingRange pos)



				
--sli = (createBB pos)
				--			 mask = genRookMask pos
					--	in (xor ((occ&mask) - 2*sli) (complement ((complement (occ&mask) - 2*(complement sli)))))
					
genPawnLeftAttacks :: BB -> BB -> BB -> BB
genPawnLeftAttacks pawns opp self = (((pawns .&. (complement (createBB [8*x|x<-[0..7]]))) `shiftL` 7) .&. (opp) .&. (complement self))

genPawnRightAttacks :: BB -> BB -> BB -> BB
genPawnRightAttacks pawns opp self = (((pawns .&. (complement (createBB [8*x+7|x<-[0..7]]))) `shiftL` 9) .&. (opp) .&. (complement self))

genPawnPushes :: BB -> BB -> BB
genPawnPushes pawns occ = (((((pawns .&. (createBB [8..15])) `shiftL` 8) .&. (complement occ)) `shiftL` 8).&. (complement occ)) .|. ((pawns `shiftL` 8) .&. (complement occ))

genPawnAttacks :: BB -> BB -> BB -> BB
genPawnAttacks pawns opp self = (genPawnLeftAttacks pawns opp self) .|. (genPawnRightAttacks pawns opp self)

genPawnMoves :: BB -> BB -> BB -> BB -> BB 
genPawnMoves pawns opp self occ = (genPawnPushes pawns occ) .|. (genPawnAttacks pawns opp self)


--rookMasks :: UArray Int BB
--rookMasks = UArray.listArray (0,63) [genRookMask i | i <- [0..63]]

-- bishopMasks :: UArray Int BB
-- bishopMasks = UArray.listArray (0,63) [genBishopMask i | i <- [0..63]]

-- rookShifts :: UArray Int Int
-- rookShifts = UArray.listArray (0,63) [64 - (popCount (rookMasks UArray.! i)) | i <- [0..63]]

-- bishopShifts :: UArray Int Int
-- bishopShifts = UArray.listArray (0,63) [64 - (popCount (bishopMasks UArray.! i)) | i <- [0..63]]

-- Just some things for debugging



iB = createBB ([0..15]++[48..63])