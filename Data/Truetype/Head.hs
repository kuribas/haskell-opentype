module Data.Truetype.Head where
import Data.Truetype.Types
import Data.Time
import Data.Int
import Data.Word

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
