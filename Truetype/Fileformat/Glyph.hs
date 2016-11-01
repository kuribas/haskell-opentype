module Data.Truetype.Glyph where
import Data.Truetype.Types
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Data.Int
import Data.Word

-- | This table contains the data that defines the appearance of
-- the glyphs in the font. This includes specification of the points
-- that describe the contours that make up a glyph outline and the
-- instructions that grid-fit that glyph. The glyf table supports the
-- definition of simple glyphs and compound glyphs, that is, glyphs
-- that are made up of other glyphs.
--
-- The glyphID order is kept when writing, but any gaps are removed.
data GlyfTable = GlyfTable (IM.IntMap Glyph)

data Glyph =
  GlyphContours [[CurvePoint]] Instructions |
  CompositeGlyph [GlyphComponent]

-- | @CurvePoint x y onCurve@: Points used to describe the outline
-- using lines and quadratic beziers.  These are relative to the
-- previous points.  If two off-curve points follow each other, an
-- on-curve point is added halfway between.
data CurvePoint = CurvePoint Int Int Bool

type Instructions = V.Vector Word8

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
