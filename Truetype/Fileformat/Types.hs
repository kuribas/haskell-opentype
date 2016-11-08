module Truetype.Fileformat.Types where
import Data.Word
import Data.Int
import Data.Bits
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as Lazy

-- | A ShortFrac is an 16 bit signed fixed number with a bias of
-- 14. This means it can represent numbers between 1.999 (0x7fff) and
-- -2.0 (0x8000). 1.0 is stored as 16384 (0x4000) and -1.0 is stored
-- as -16384 (0xc000).  Efficient numeric instances are provided.
newtype ShortFrac = ShortFrac Int16
  
-- | signed fixed-point number
type Fixed = Word32
-- | FWord describes a quantity in FUnits, the smallest
-- measurable distance in em space.
type FWord = Int16
-- | UFWord describes a quantity in FUnits, the smallest measurable
-- distance in em space.
type UFWord = Word16
-- | the glyph index in the glyph table
type GlyphID = Word16

-- return smallest power of 2 <= i 
iLog2 :: Integral a => a -> a
iLog2 = iLog2' 0 where
  iLog2' base i
    | i > 0 = iLog2' (base+1) (i `quot` 2) 
    | otherwise = base-1

byteAt :: (Bits a, Num a) => a -> Int -> Bool
byteAt flag i = flag .&. 1 `shift` i /= 0
{-# SPECIALIZE byteAt :: Word8 -> Int -> Bool #-}
{-# SPECIALIZE byteAt :: Word16 -> Int -> Bool #-}

makeFlag :: [Bool] -> Word16
makeFlag l =
  fromIntegral $ sum $ zipWith (*) (iterate (*2) 1) $
  map fromEnum l

instance Num ShortFrac where
  (ShortFrac a) + (ShortFrac b) = ShortFrac $ a + b
  (ShortFrac a) - (ShortFrac b) = ShortFrac $ a - b
  (ShortFrac a) * (ShortFrac b) =
    ShortFrac $ fromIntegral (((fromIntegral a :: Int32) * (fromIntegral b :: Int32)) `shift` (-14))
  abs (ShortFrac a) = ShortFrac $ abs a
  fromInteger i = ShortFrac $ fromIntegral i `shift` 14
  signum (ShortFrac a) = fromIntegral $ signum a

instance Eq ShortFrac where
  (ShortFrac a) == (ShortFrac b) = a == b

instance Ord ShortFrac where
  compare (ShortFrac a) (ShortFrac b) = compare a b

instance Fractional ShortFrac where
  fromRational r =
    ShortFrac $ fromIntegral $ 
    floor ((r+2) * 0x4000) - (0x8000::Word16)
  (ShortFrac a) / (ShortFrac b) =
    ShortFrac $ fromIntegral $
    ((fromIntegral a :: Int32) `shift` 14) `quot` fromIntegral b

instance Show ShortFrac where
  show a = show (realToFrac a :: Float)

instance Real ShortFrac where
  toRational (ShortFrac a) =
    fromIntegral ((fromIntegral a::Word16) + 0x8000) / 0x4000 - 2

instance RealFrac ShortFrac where
  properFraction (ShortFrac a)
    | a < 0 && f /= 0 = (i+1, ShortFrac (-f))
    | otherwise = (i, ShortFrac f)
    where i = fromIntegral (((fromIntegral a :: Word16) + 0x8000) `shift` (-14)) - 2
          f = a .&. 0x3fff

putShortFrac :: ShortFrac -> Put
putShortFrac (ShortFrac a) = putInt16be a

safeGetTable :: Lazy.ByteString -> Int -> Get a -> Either (Lazy.ByteString, ByteOffset, String) (Lazy.ByteString, a)
safeGetTable bStr size getter = do
  (rest, offset, res) <- runGetOrFail getter bStr
  if fromIntegral offset > size
    then Left (rest, offset, "Error: subtable read past boundary")
    else Right (Lazy.drop (fromIntegral size - offset) rest, res)

getWithBounds :: Lazy.ByteString -> [Int] -> Get a -> Either (Lazy.ByteString, ByteOffset, String) [a]
getWithBounds _ [] _ = Right []
getWithBounds bStr (b:bs) getter = do
  (rest, res) <- safeGetTable bStr b getter
  (res:) <$> getWithBounds rest bs getter
