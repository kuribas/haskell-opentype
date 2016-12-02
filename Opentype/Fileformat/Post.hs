{-# LANGUAGE LambdaCase #-}
module Opentype.Fileformat.Post where
import Opentype.Fileformat.Types
import Data.Word
import Data.Char
import Data.Binary.Put
import Data.Binary.Get
import Data.Foldable
import Control.Monad (replicateM, replicateM_, when)

data PostVersion =
  -- | The first 258 Glyphs have standard names
  PostTable1 |
  -- | Order of glyph names can be changed, and glyphs can have
  -- non-standard names.
  PostTable2 |
  -- | No glyph names
  PostTable3
  deriving (Eq, Show)

-- |This table contains additional information needed to use TrueType
-- or OpenType™ fonts on PostScript printers. This includes data for
-- the FontInfo dictionary entry and the PostScript names of all the
-- glyphs. For more information about PostScript names, see the Adobe
-- document Unicode and Glyph Names.
--
-- Versions 1.0, 2.0, and 2.5 refer to TrueType fonts and OpenType
-- fonts with TrueType data. OpenType fonts with TrueType data may
-- also use Version 3.0. OpenType fonts with CFF data use Version 3.0
-- only.
data PostTable = PostTable {
  postVersion :: PostVersion,
  -- | Italic angle in counter-clockwise degrees from the
  -- vertical. Zero for upright text, negative for text that leans to
  -- the right (forward).
  italicAngle :: Fixed,
  -- | This is the suggested distance of the top of the underline from
  -- the baseline (negative values indicate below baseline).
  --
  -- The PostScript definition of this FontInfo dictionary key (the y
  -- coordinate of the center of the stroke) is not used for
  -- historical reasons. The value of the PostScript key may be
  -- calculated by subtracting half the underlineThickness from the
  -- value of this field.
  underlinePosition :: FWord,
  -- | suggested values for the underline thickness.
  underlineThickness :: FWord,
  -- | Set to 0 if the font is proportionally spaced, non-zero if the
  -- font is not proportionally spaced (i.e. monospaced).
  isFixedPitch :: Word32,
  -- | Minimum memory usage when an OpenType font is downloaded.  Set
  -- to 0 if unsure.
  minMemType42 :: Word32,
  -- | Maximum memory usage when an OpenType font is downloaded.  Set
  -- to 0 if unsure.
  maxMemType42 :: Word32,
  -- | Minimum memory usage when an OpenType font is downloaded as a
  -- Type 1 font.  Set to 0 if unsure.
  minMemType1 :: Word32,
  -- | Maximum memory usage when an OpenType font is downloaded as a
  -- Type 1 font.  Set to 0 if unsure.
  maxMemType1 :: Word32,
  -- | Ordinal number of the glyph in 'post' string tables.  For
  -- format 2.0 only.
  -- 
  -- If the name index is between 0 and 257, treat the name index as a
  -- glyph index in the Macintosh standard order. If the name index is
  -- between 258 and 65535, then subtract 258 and use that to index
  -- into the list of Pascal strings at the end of the table. Thus a
  -- given font may map some of its glyphs to the standard glyph
  -- names, and some to its own names.
  --
  -- If you do not want to associate a PostScript name with a
  -- particular glyph, use index number 0 which points to the name
  -- .notdef.
  glyphNameIndex :: [Int],
  -- | strings for indices 258 and upwards.
  postStrings :: [String]
  }
  deriving (Show)

getPostTable :: Get PostTable
getPostTable = do
  iVersion <- getWord32be
  version <- case iVersion of
    0x00010000 -> return PostTable1
    0x00020000 -> return PostTable2
    0x00030000 -> return PostTable3
    _ -> fail "Unsupported post table version."
  itA <- getWord32be
  ulPos <- getInt16be
  ulThick <- getInt16be
  isFixed <- getWord32be
  min42 <- getWord32be
  max42 <- getWord32be
  min1 <- getWord32be
  max1 <- getWord32be
  if version /= PostTable2
    then return $ PostTable version itA ulPos
         ulThick isFixed min42 max42 min1 max1 [] []
    else do
      n <- fromIntegral <$> getWord16be
      gIndex <- replicateM n (fromIntegral <$> getWord16be)
      pStrings <- replicateM ((maximum gIndex+1)-258) $ do
        l <- fromIntegral <$> getWord8
        replicateM l ((chr.fromIntegral) <$> getWord8)
      return $ PostTable version itA ulPos
         ulThick isFixed min42 max42 min1 max1 gIndex pStrings

putPostTable :: PostTable -> Put
putPostTable table = do
  putWord32be $ case postVersion table of
    PostTable1 -> 0x00010000
    PostTable2 -> 0x00020000
    PostTable3 -> 0x00030000
  putWord32be $ italicAngle table
  putInt16be $ underlinePosition table
  putInt16be $ underlineThickness table
  putWord32be $ isFixedPitch table
  putWord32be $ minMemType42 table
  putWord32be $ maxMemType42 table
  putWord32be $ minMemType1 table
  putWord32be $ maxMemType1 table
  when (postVersion table == PostTable2) $ do
    let nameIndexLen = length (glyphNameIndex table)
        nameMax = maximum (glyphNameIndex table) + 1
        postLen = length (postStrings table)
    putWord16be $ fromIntegral nameIndexLen
    traverse_ (putWord16be.fromIntegral) $ glyphNameIndex table
    for_ (take (nameMax-258) $ postStrings table) $ \str -> do
      putWord8 $ fromIntegral $ length str
      traverse_ (putWord8.fromIntegral.ord) str
    replicateM_ (nameMax-258-postLen) (putWord8 0)
    
