module Truetype.Fileformat.Types where
import Data.Word
import Data.Int
import Data.Bits

-- | A ShortFrac is an Int16 with a bias of 14. This means it can
-- represent numbers between 1.999 (0x7fff) and -2.0 (0x8000). 1.0 is
-- stored as 16384 (0x4000) and -1.0 is stored as -16384 (0xc000).
type ShortFrac = Int16
-- | signed fixed-point number
type Fixed = Word32
-- | FWord describes a quantity in FUnits, the smallest
-- measurable distance in em space.
type FWord = Int16
-- | UFWord describes a quantity in FUnits, the smallest measurable
-- distance in em space.
type UFWord = Word16
-- | fixed number with the low 14 bits representing fraction.
type F2Dot14 = Int16
-- | the glyph index in the glyph table
type GlyphID = Word16

-- return smallest power of 2 <= i 
iLog2 :: Integral a => a -> a
iLog2 = iLog2' 0 where
  iLog2' base i
    | i > 0 = iLog2' (base+1) (i `quot` 2) 
    | otherwise = base-1

byteAt :: (Bits a, Num a) => a -> Int -> Bool
byteAt flag i = flag `shift` i /= 0
{-# SPECIALIZE byteAt :: Word8 -> Int -> Bool #-}
{-# SPECIALIZE byteAt :: Word16 -> Int -> Bool #-}

makeFlag :: [Bool] -> Word16
makeFlag l =
  fromIntegral $ sum $ zipWith (*) (iterate (*2) 1) $
  map fromEnum l
