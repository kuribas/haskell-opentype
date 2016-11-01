module Truetype.Fileformat
       (-- * types
         ShortFrac, Fixed, FWord, UFWord, F2Dot14, GlyphID,
         -- * Main datatype
         TruetypeFont (..),
         -- * Head table
         HeadTable(..),
         -- * Glyf table
         GlyfTable(..), Glyph(..), CurvePoint(..), Instructions, GlyphComponent(..),
         -- * CMap table
         CmapTable(..), CMap(..), PlatformID(..)
       ) where
import Truetype.Types
import Truetype.Head
import Truetype.Glyph
import Truetype.Cmap
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

data SFont = TruetypeSFont TruetypeFont |
             OpentypeSFont OpentypeFont |
             OtherSfont OtherFont

data OpentypeTables = OpentypeTables {
  head :: HeadTable,
  cmap :: CmapTable,
  hhea :: HheaTable,
  hmtx :: HmtxTable,
  maxp :: MaxpTable,
  name :: NameTable,
  post :: PostTable,
  os2 :: Maybe OS2Table,  
  otherTables :: GenericTables
  }

data TruetypeFont = TruetypeFont {
  ttCommon :: OpentypeTables,
  glyf :: GlyfTable,
  fpgm :: Maybe FpgmTable
  }

data OpentypeFont = OpentypeFont {
  otCommon :: OpentypeTables,
  cff :: CffTable
  }

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
