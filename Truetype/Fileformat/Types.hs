module Truetype.Fileformat.Types where
import Data.Word
import Data.Int

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

type GlyphID = Word16

