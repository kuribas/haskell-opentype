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
         GlyfTable(..), Glyph(..), GlyphOutlines(..),
         CurvePoint(..), Instructions, GlyphComponent(..),
         -- * CMap table
         CmapTable(..), CMap(..), PlatformID(..), MapFormat (..),
         -- * Maxp table
         MaxpTable(..),
         -- * Name table
         NameTable(..), NameRecord(..),
         -- * Post table
         PostTable(..)
       ) where
import Truetype.Fileformat.Types
import Truetype.Fileformat.Head
import Truetype.Fileformat.Glyph
import Truetype.Fileformat.Cmap
import Truetype.Fileformat.Maxp
import Truetype.Fileformat.Name
import Truetype.Fileformat.Post
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Data.Bits
import Data.List (zip4)
import Data.Char
import Data.Foldable
import Control.Monad
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as M

type GenericTables = M.Map String Lazy.ByteString

-- | truetype or opentype font
data OpentypeFont = OpentypeFont {
  -- | Use apple scaler.  Should not be used for opentype fonts.
  appleScaler :: Bool,
  -- | global information about the font. 
  headTbl :: HeadTable,
  -- | mapping of character codes to the glyph index values
  cmap :: CmapTable,
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
  glyf :: GlyfTable
  -- font hinting programs
  -- fpgm :: Maybe FpgmTable
  }

-- | tables for cubic outlines (opentype)
data CubicTables = CubicTables {
  -- cff :: CffTable
  }

data OS2Table = OS2Table

data ScalerType =
  -- opentype with cff
  CubicScaler |
  -- truetype and opentype with glyf
  QuadScaler |
  -- apple only scaler
  AppleScaler
  
type SfntLocs = M.Map Scaler (Word32, Word32)
type Scaler = Word32

nameToInt :: String -> Word32
nameToInt string =
  fromIntegral $ sum $ zipWith (\c b -> ord c `shift` b) string [24, 16..0]

readTables :: Get (SfntLocs, ScalerType)
readTables = do
  scaler <- getWord32be
  scalerType <- case scaler of
    0x74727565 -> return AppleScaler
    0x4F54544F -> return CubicScaler
    0x00010000 -> return QuadScaler
    _ -> fail "This file is not a truetype or opentype file."
  numTables <- getWord16be
  skip 6
  locs <- fmap M.fromAscList $
          replicateM (fromIntegral numTables) $
    do tag <- getWord32be
       _ <- getWord32be
       offset <- getWord32be
       size <- getWord32be
       return (tag, (offset, size))
  return (locs, scalerType)

checkSum :: Lazy.ByteString -> Word32
checkSum bs = 
  runGet ((fromIntegral.sum) <$>
           replicateM (size `quot` 4) getWord32be)
  bs
  where
    size = fromIntegral $ Lazy.length bs

headWithChecksum :: Lazy.ByteString -> Word32 -> Put
headWithChecksum bs cksum = do
  putLazyByteString $ Lazy.take 8 bs
  putWord32be $ 0xB1B0AFBA - cksum
  putLazyByteString $ Lazy.drop 12 bs
  
padTable :: PutM Int -> PutM Int
padTable p = do
  sz <- p
  let padded = (sz+3) .&. complement 3
  replicateM_ (padded-sz) (putWord8 0)
  return padded
    
writeTables :: ScalerType -> [(Word32, PutM Int)] -> Put
writeTables scaler tables = do
  putLazyByteString unChecked
  let cksumTot = fromIntegral $ sum $ checkSum unChecked:ckSums
  for_ (zip tables tableBs) $
    \((tag,_), bs) -> 
      if tag == nameToInt "head"
        then headWithChecksum bs cksumTot
        else putLazyByteString bs
  where
    entrySelector, searchRange, nTables :: Word16
    nTables = fromIntegral $ length tables
    entrySelector = fromIntegral $ iLog2 nTables
    searchRange = 1 `shift` (fromIntegral entrySelector+4)
    offsets = scanl (+) (fromIntegral $ 16*length tables + 12) lengths
    (lengths, tableBs) = unzip $ map (runPutM.padTable.snd) tables
    ckSums = map checkSum tableBs
    unChecked = runPut $ do
      putWord32be $ case scaler of
        AppleScaler -> 0x74727565
        CubicScaler -> 0x4F54544F
        QuadScaler -> 0x00010000
      putWord16be $ fromIntegral nTables
      putWord16be searchRange
      putWord16be entrySelector
      putWord16be $ nTables * 16 - searchRange
      for_ (zip4 tables ckSums offsets lengths) $
        \((tag,_), cksum, offset, len) -> do
          putWord32be tag
          putWord32be cksum
          putWord32be $ fromIntegral offset
          putWord32be $ fromIntegral len
