-- | This module provides Truetype and Opentype loading and writing.
-- An attempt was made to have a higher level interface without
-- sacrificing features of the file format.

module Truetype.Fileformat
       (-- * types
         ShortFrac (..), Fixed, FWord, UFWord, GlyphID,
         -- * Main datatype
         OpentypeFont (..), OutlineTables (..), QuadTables (..),
         CubicTables (..), GenericTables,
         -- * Head table
         HeadTable(..),
         -- * Glyf table
         GlyfTable(..), Glyph(..),
         CurvePoint(..), Instructions, GlyphComponent(..),
         -- * CMap table
         CmapTable(..), CMap(..), PlatformID(..), MapFormat (..)
       ) where
import Truetype.Fileformat.Types
import Truetype.Fileformat.Head
import Truetype.Fileformat.Glyph
import Truetype.Fileformat.Cmap
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as M

type GenericTables = M.Map String Lazy.ByteString

-- | opentype font
data OpentypeFont = OpentypeFont {
  -- | global information about the font. 
  head :: HeadTable,
  -- | mapping of character codes to the glyph index values
  cmap :: CmapTable,
  -- | horizontal data for glyphs (calculated from glyphs)
  hhea :: HheaTable,
  -- | information strings in different languages
  name :: NameTable,
  -- | data for postscript printers
  post :: PostTable,
  -- | windows specific information
  os2 :: Maybe OS2Table,
  -- | tables specific to the outline type (cubic or quadratic).
  outlineTables :: OutlineTables,
  -- | not (yet) supported tables
  otherTables :: GenericTables
  }

-- | outline tables
data OutlineTables =
  QuadOutline QuadTables |
  CubicOutline CubicTables

-- | tables for quadratic outlines (truetype or opentype)
data QuadTables = QuadTables {
  -- | maximum values for storage allocation
  maxp :: MaxpTable,  
  -- | table of glyphs
  glyf :: GlyfTable,
  -- | font hinting programs
  fpgm :: Maybe FpgmTable
  }

-- | tables for cubic outlines (opentype)
data CubicTables = CubicTables {
  
  cff :: CffTable
  }

data HheaTable = HheaTable
data HmtxTable = HmtxTable
data MaxpTable = MaxpTable
data NameTable = NameTable
data OS2Table = OS2Table
data PostTable = PostTable
data CffTable = CffTable
data FpgmTable = FpgmTable
