module Data.TrueType
       (-- * types
         ShortFrac, Fixed, FWord, UFWord, F2Dot14, GlyphID,
         -- * Main datatype
         ScalerType (..), TrueTypeFont (..),
         -- * Head table
         HeadTable(..),
         -- * Glyf table
         GlyfTable(..), Glyph(..), CurvePoint(..), Instructions, GlyphComponent(..),
         -- * CMap table
         CmapTable(..), CMap(..), PlatformID(..), RangeSegment(..), RangeSegment32(..), GlyphMap(..)
       ) where
import Data.Int
import Data.Word
import Data.ByteString
import Data.Time
import Data.Map
import Data.IntMap
import Data.Vector

-- | A ShortFrac is an Int16 with a bias of 14. This means it can
-- represent numbers between 1.999 (0x7fff) and -2.0 (0x8000). 1.0 is
-- stored as 16384 (0x4000) and -1.0 is stored as -16384 (0xc000).
type ShortFrac = Int16
-- | signed fixed-point number
type Fixed = Word16
-- | FWord describes a quantity in FUnits, the smallest
-- measurable distance in em space.
type FWord = Int16
-- | UFWord describes a quantity in FUnits, the smallest measurable
-- distance in em space.
type UFWord = Word16
-- | fixed number with the low 14 bits representing fraction.
type F2Dot14 = Int16

type GlyphID = Word16

data ScalerType =
  -- | for TrueType fonts  
  TrueTypeScaler |
  -- | for old style of PostScript font housed in a sfnt wrapper
  Type1Scaler |
  -- | an OpenType font with PostScript outlines
  OpenTypeScaler

data TrueTypeFont = TTOutlineFont {
  scaler :: ScalerType,
  head :: HeadTable,
  cmap :: CmapTable,
  glyf :: GlyfTable,
  hhea :: HheaTable,
  hmtx :: HmtxTable,
  maxp :: MaxpTable,
  name :: NameTable,
  post :: PostTable,
  othertables :: Map String ByteString} |
                    TTBitmapFont {
  scaler :: ScalerType,
  head :: HeadTable,
  cmap :: CmapTable }
  
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
  
  -- bit 15 zero
  lastResortFont :: Bool,
  -- | Valid range is from 16 to 16384. This value should be a power
  -- of 2 for fonts that have TrueType outlines.
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
  -- | deprecated, set to 2
  fontDirectionHint :: Word16,
  -- | 0 for short offsets, 1 for long.
  indexToLocFormat :: Word16,
  -- | 0 for current format
  glyphDataFormat :: Int16
  }

-- | This table contains the data that defines the appearance of
-- the glyphs in the font. This includes specification of the points
-- that describe the contours that make up a glyph outline and the
-- instructions that grid-fit that glyph. The glyf table supports the
-- definition of simple glyphs and compound glyphs, that is, glyphs
-- that are made up of other glyphs.
data GlyfTable = GlyfTable (Vector Glyph)

data Glyph =
  GlyphContours [[CurvePoint]] Instructions |
  CompositeGlyph [GlyphComponent]

-- | @CurvePoint x y onCurve@: Points used to describe the outline
-- using lines and quadratic beziers.  These are relative to the
-- previous points.  If two off-curve points follow each other, an
-- on-curve point is added halfway between.
data CurvePoint = CurvePoint Int Int Bool

newtype Instructions =
  Vector Word8

data GlyphComponent =
  GlyphComponent {
  componentID :: GlyphID,
  componentInstructions :: Maybe Instructions,
  componentXX :: F2Dot14, -- ^ transformation matrix for scaling the glyph
  componentXY :: F2Dot14, -- ^ transformation matrix for scaling the glyph
  componentYX :: F2Dot14, -- ^ transformation matrix for scaling the glyph
  componentYY :: F2Dot14, -- ^ transformation matrix for scaling the glyph
  componentX :: Int, -- ^ if matchPoints is `True`, index of matching
                     -- point in compound being constructed, otherwise
                     -- x shift.  This offset is unscaled (microsoft
                     -- bit set) in the rasterizer.
  componentY :: Int, -- ^ if matchPoints is `True`, index of matching
                     -- point in compound, otherwise y shift.  This
                     -- offset is unscaled (microsoft bit set) in the
                     -- rasterizer.
  matchPoints :: Bool, -- ^ see previous
  useMyMetrics :: Bool, -- ^ Use metrics from this component for the compound glyph.
  overlapCompound :: Bool -- ^ If set, the components of this compound glyph overlap
}

-- | This table defines the mapping of character codes to the glyph
-- index values used in the font. It may contain more than one
-- subtable, in order to support more than one character encoding
-- scheme. Character codes that do not correspond to any glyph in the
-- font should be mapped to glyph index 0. The glyph at this location
-- must be a special glyph representing a missing character, commonly
-- known as .notdef.
--
-- The table header indicates the character encodings for which
-- subtables are present. Each subtable is in one of seven possible
-- formats and begins with a format code indicating the format
-- used.
-- 
-- The `platformID` and platform-specific `encodingID` in the header
-- entry (and, in the case of the Macintosh platform, the `macLanguage`
-- field in the subtable itself) are used to specify a particular
-- cmap encoding. Each platform ID, platform-specific encoding ID,
-- and subtable `macLanguage` combination may appear only once in the
-- `CmapTable`.
--
-- When `platformID` is `UnicodePlatform`, `encodingID` is interpreted as follows:
-- 
--  * 0: Default semantics
--  * 1: Version 1.1 semantics
--  * 2: ISO 10646 1993 semantics (deprecated)
--  * 3: Unicode 2.0 or later semantics (BMP only)
--  * 4: Unicode 2.0 or later semantics (non-BMP characters allowed)
--  * 5: Unicode Variation Sequences
--  * 6: Full Unicode coverage (used with type 13.0 cmaps by OpenType)
--
-- When `platformID` `MacintoshPlatform`, the `encodingID` is a QuickDraw script code.
-- 
-- Note that the use of the Macintosh platformID is currently
-- discouraged. Subtables with a Macintosh platformID are only
-- required for backwards compatibility with QuickDraw and will be
-- synthesized from Unicode-based subtables if ever needed.
--
-- When `platformID` is `MicrosoftPlatform`, the `encodingID` is a is interpreted as follows:
--
-- * 0: Symbol
-- * 1: Unicode BMP-only (UCS-2)
-- * 2: Shift-JIS
-- * 3: PRC
-- * 4: BigFive
-- * 5: Johab
-- * 10: Unicode UCS-4


data CmapTable = CmapTable [CMap]

data PlatformID =
  UnicodePlatform |
  -- | DEPRECATED
  MacintoshPlatform |
  MicrosoftPlatform

data CMap = CMap {
  platformID :: PlatformID,
  encodingID :: Word16,
  -- | used only in the Macintosh platformID (DEPRECATED)
  macLanguage :: Word16,
  glyphMap :: GlyphMap}

data RangeSegment = RangeSegment {
  rangeStartCode :: Word16,
  rangeEndCode :: Word16,
  idDelta :: Int16,
  idRangeOffset :: Word16 }

data RangeSegment32 = RangeSegment32 {
  startCharCode :: Word32,
  endCharCode :: Word32,
  startGlyphCode :: Word32 }


data GlyphMap =
  -- | array of the first 256 glyphs (DEPRECATED)
  MapFormat1 (Vector GlyphID) |
  -- | mixed 8/16 bit encoding (DEPRECATED)
  MapFormat2 (IntMap RangeSegment) (Vector GlyphID) |
  -- | 16 bit encoding with holes
  MapFormat4 (IntMap RangeSegment) (Vector GlyphID) |
  -- | @MapFormat6 firstCode entryCount glyphIndexArray@: trimmed 16
  -- bit mapping.
  MapFormat6 Word16 Word16 (Vector GlyphID) |
  -- | mixed 16/32 bit encoding (DEPRECATED)
  MapFormat8 |
  -- | 32 bit segmented coverage
  MapFormat12 (IntMap RangeSegment32)

data HheaTable = HheaTable
data HmtxTable = HmtxTable
data MaxpTable = MaxpTable
data NameTable = NameTable
data PostTable = PostTable
