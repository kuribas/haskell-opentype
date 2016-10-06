module Data.Truetype
       (-- * types
         ShortFrac, Fixed, FWord, UFWord, F2Dot14, GlyphID,
         -- * Main datatype
         ScalerType (..), TrueTypeFont (..),
         -- * Head table
         HeadTable(..),
         -- * Glyf table
         GlyfTable(..), Glyph(..), CurvePoint(..), Instructions, GlyphComponent(..),
         -- * CMap table
         CmapTable(..), CMap(..), PlatformID(..)
       ) where
import Data.Truetype.Types
import Data.Truetype.Head
import Data.Truetype.Glyph
import Data.Truetype.Cmap
import Data.Int
import Data.List (sort)
import Data.Word
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Time
import qualified Data.Map as M
import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

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
  othertables :: M.Map String BS.ByteString} |
                    TTBitmapFont {
  scaler :: ScalerType,
  head :: HeadTable,
  cmap :: CmapTable }
  

    

data HheaTable = HheaTable
data HmtxTable = HmtxTable
data MaxpTable = MaxpTable
data NameTable = NameTable
data PostTable = PostTable
