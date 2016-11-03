module Data.Truetype.Head (HeadTable(..)) where
import Data.Truetype.Types
import Data.Time
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Data.Int
import Data.Bits


-- | This table contains global information about the font. it
-- records such facts as the font version number, the creation and
-- modification dates, revision number and basic typographic data that
-- applies to the font as a whole. this includes a specification of
-- the font bounding box, the direction in which the font's glyphs are
-- most likely to be written and other information about the placement
-- of glyphs in the em square.
data HeadTable = HeadTable {
  -- | 0x00010000 for version 1.0.
  version :: Fixed,
  -- | set by font manufacturer.
  fontRevision :: Fixed,
  -- | baseline for font at y=0
  baselineYZero :: Bool,
  -- |  left sidebearing point at x=0;
  sidebearingXZero :: Bool,
  -- | instructions may depend on point size;
  pointsizeDepend :: Bool,
  -- | Force ppem to integer values for all internal scaler math; may
  -- use fractional ppem sizes if this bit is clear;
  integerScaling :: Bool,  
  -- | /Microsoft/: Instructions may alter advance width (the advance widths might not scale linearly);
  alterAdvanceWidth :: Bool,
  -- | /Apple/: This bit should be set in fonts that are intended to e
  -- laid out vertically, and in which the glyphs have been drawn such
  -- that an x-coordinate of 0 corresponds to the desired vertical
  -- baseline.
  
  -- bit 6 zero
  verticalFont :: Bool,
  -- | /Apple/: This should be set if the font requires layout for
  -- correct linguistic rendering (e.g. Arabic fonts).
  linguisticRenderingLayout :: Bool,
  -- | /Apple/: This should be set for an AAT font which has one or more
  -- metamorphosis effects designated as happening by default.
  metamorphosisEffects :: Bool,
  -- | his bit should be set if the font contains any strong
  -- right-to-left glyphs.
  rightToLeftGlyphs :: Bool,
  -- | This bit should be set if the font contains Indic-style
  -- rearrangement effects.
  indicRearrangements :: Bool,
  -- | /Adobe/: Font data is ‘lossless’ as a results of having been
  -- subjected to optimizing transformation and/or compression (such
  -- as e.g. compression mechanisms defined by ISO/IEC 14496-18,
  -- MicroType Express, WOFF 2.0 or similar) where the original font
  -- functionality and features are retained but the binary
  -- compatibility between input and output font files is not
  -- guaranteed. As a result of the applied transform, the ‘DSIG’
  -- Table may also be invalidated.
  losslessFontData :: Bool,
  -- | /Adobe/: Font converted (produce compatible metrics)
  convertedFont :: Bool,
  -- | /Adobe/: Font optimized for ClearType™. Note, fonts that rely on
  -- embedded bitmaps (EBDT) for rendering should not be considered
  -- optimized for ClearType, and therefore should keep this bit
  -- cleared.
  clearTypeOptimized :: Bool,
    -- | Last Resort font. If set, indicates that the glyphs encoded in
  -- the cmap subtables are simply generic symbolic representations of
  -- code point ranges and don’t truly represent support for those
  -- code points. If unset, indicates that the glyphs encoded in the
  -- cmap subtables represent proper support for those code points.
  lastResortFont :: Bool,
  -- | Valid range is from 16 to 16384. This value should be a power
  -- of 2 for fonts that have TrueType outlines.

  -- bit 15 zero
  unitsPerEm :: Word16,
  created :: UTCTime,  modified :: UTCTime,
  -- | For all glyph bounding boxes
  xMin :: FWord,
  -- | For all glyph bounding boxes
  yMin :: FWord,
  -- | For all glyph bounding boxes
  xMax :: FWord,
  -- | For all glyph bounding boxes
  yMax :: FWord,
  -- macStyle
  
  boldStyle :: Bool,
  italicStyle :: Bool,
  underlineStyle :: Bool,
  outlineStyle :: Bool,
  shadowStyle :: Bool,
  condensedStyle :: Bool,
  extendedStyle :: Bool,
  -- | Smallest readable size in pixels.
  lowerRecPPEM :: Word16,
  -- | deprecated, will be set to 2
  fontDirectionHint :: Int16,
  -- | 0 for short offsets, 1 for long.  Will be automatically written.
  indexToLocFormat :: Int16,
  -- | 0 for current format
  glyphDataFormat :: Int16
  }

instance Binary HeadTable where
  get = do
    major <- getWord16be
    minor <- getWord16be
    when (major /= 1 && minor /= 0)
      (fail "Invalid head table")
    revision <- getWord32be
    _ <- getWord32be
    magic <- getWord32be
    when (magic /= 0x5F0F3CF5)
      (fail "Invalid magic value in head table")
    flags <- getWord16be
    uPe <- getWord16be
    created_ <- getInt64be
    modified_ <- getInt64be
    xMin_ <- getInt16be
    yMin_ <- getInt16be
    xMax_ <- getInt16be
    yMax_ <- getInt16be
    mcStyle <- getWord16be
    lRec <- getWord16be
    fDir <- getInt16be
    iToL <- getInt16be
    gd <- getInt16be
    let flagAt = byteAt flags
        styleAt = byteAt mcStyle
    return $ HeadTable 0x00010000 revision
      (flagAt 0) (flagAt 1) (flagAt 2) (flagAt 3)
      (flagAt 4) (flagAt 5) (flagAt 7) (flagAt 8) (flagAt 9)
      (flagAt 10) (flagAt 11) (flagAt 12) (flagAt 13) (flagAt 14) 
      uPe (getTime created_) (getTime modified_)
      xMin_ yMin_ xMax_ yMax_
      (styleAt 0) (styleAt 1) (styleAt 2) (styleAt 3) (styleAt 4)
      (styleAt 5) (styleAt 6) lRec fDir iToL gd

  put headTbl = do
  putWord16be 1
  putWord16be 0
  putWord32be $ fontRevision headTbl
  putWord32be 0
  putWord32be 0x5F0F3CF5
  putWord16be $ makeFlag $ map ($ headTbl)
    [baselineYZero, sidebearingXZero, pointsizeDepend, integerScaling, alterAdvanceWidth,
    const False, verticalFont, linguisticRenderingLayout, metamorphosisEffects, rightToLeftGlyphs,
    indicRearrangements, losslessFontData, convertedFont, clearTypeOptimized, lastResortFont, const False]
  putWord16be $ unitsPerEm headTbl
  putInt64be $ putTime $ created headTbl
  putInt64be $ putTime $ modified headTbl
  putInt16be $ xMin headTbl
  putInt16be $ yMin headTbl
  putInt16be $ xMax headTbl
  putInt16be $ yMax headTbl
  putWord16be $ makeFlag $ map ($ headTbl)
    [boldStyle, italicStyle, underlineStyle, shadowStyle, condensedStyle, extendedStyle]
  putWord16be $ lowerRecPPEM headTbl
  putInt16be $ fontDirectionHint headTbl
  putInt16be $ indexToLocFormat headTbl
  putInt16be $ glyphDataFormat headTbl

byteAt :: Word16 -> Int -> Bool
byteAt flag i = flag `shift` i /= 0      

makeFlag :: [Bool] -> Word16
makeFlag l =
  fromIntegral $ sum $ zipWith (*) (iterate (*2) 1) $
  map fromEnum l

secDay :: Int64
secDay = 60 * 60 * 24

diffSeconds :: Int64
diffSeconds =
  secDay * fromIntegral (fromGregorian 1858 11 17 `diffDays` fromGregorian 1904 1 1)

getTime :: Int64  -> UTCTime
getTime secs = UTCTime (ModifiedJulianDay $ fromIntegral d) (secondsToDiffTime $ fromIntegral t)
  where (d,t) = (secs - diffSeconds) `quotRem` fromIntegral secDay

putTime :: UTCTime -> Int64
putTime (UTCTime (ModifiedJulianDay d) t) =
  fromIntegral d * secDay + diffTimeToSeconds t + diffSeconds

diffTimeToSeconds :: DiffTime -> Int64
diffTimeToSeconds d =
  fromIntegral $ diffTimeToPicoseconds d `quot` 1000000000

