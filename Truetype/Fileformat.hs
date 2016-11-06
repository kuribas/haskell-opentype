-- | This module provides Truetype and Opentype loading and writing.
-- An attempt was made to have a higher level interface without
-- sacrificing features of the file format.

module Truetype.Fileformat
       (-- * types
         ShortFrac, Fixed, FWord, UFWord, F2Dot14, GlyphID,
         -- * Main datatype
         SFont (..), CommonTables (..), TruetypeFont (..),
         OpentypeFont (..), OtherFont (..), GenericTables (..),
         -- * Head table
         HeadTable(..),
         -- * Glyf table
         GlyfTable(..), Glyph(..), advanceWidth, leftSideBearing,
         CurvePoint(..), Instructions, GlyphComponent(..),
         -- * CMap table
         CmapTable(..), CMap(..), PlatformID(..), MapFormat (..)
       ) where
import Truetype.Fileformat.Types
import Truetype.Fileformat.Head
import Truetype.Fileformat.Glyph
import Truetype.Fileformat.Cmap
import Data.Int
import Data.List (sort)
import Data.Word
import qualified Data.ByteString.Lazy as Lazy
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Time
import qualified Data.Map as M
import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

type GenericTables = M.Map String Lazy.ByteString

-- | supported fonts in sfnt wrapper
data SFont =
  -- | Truetype font or Opentype font with quadratic outlines
  TruetypeSFont TruetypeFont |
  -- | Opentype font with cubic outlines.
  OpentypeSFont OpentypeFont |
  -- | Unsupported font
  OtherSfont OtherFont

-- | common tables for truetype and opentype
data CommonTables = CommonTables {
  -- | global information about the font. 
  head :: HeadTable,
  -- | mapping of character codes to the glyph index values
  cmap :: CmapTable,
  -- | horizontal data for glyphs (calculated from glyphs)
  hhea :: HheaTable,
  -- | global horizontal metrics
  hmtx :: HmtxTable,
  -- | maximum values for storage  allocation
  maxp :: MaxpTable,
  -- | various font information in different languages
  name :: NameTable,
  -- | data for postscript printers
  post :: PostTable,
  -- | windows specific information
  os2 :: Maybe OS2Table,
  -- | not (yet) supported tables
  otherTables :: GenericTables
  }

-- | truetype (quadratic outlines) tables
data TruetypeFont = TruetypeFont {
  ttCommon :: CommonTables,
  -- | (quadratic) glyf descriptions
  glyf :: GlyfTable,
  -- | font hinting programs
  fpgm :: Maybe FpgmTable
  }

-- | opentype (cubic outlines) tables
data OpentypeFont = OpentypeFont {
  otCommon :: CommonTables,
  cff :: CffTable
  }

-- | unsupported font
data OtherFont =
  OtherFont GenericTables

data HheaTable = HheaTable
data HmtxTable = HmtxTable
data MaxpTable = MaxpTable
data NameTable = NameTable
data OS2Table = OS2Table
data PostTable = PostTable
data CffTable = CffTable
data FpgmTable = FpgmTable
