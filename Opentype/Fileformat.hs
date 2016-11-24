{-# LANGUAGE TupleSections, TemplateHaskell #-}

-- | This module provides opentype file loading and writing.  An
-- attempt was made to have a higher level interface without
-- sacrificing features of the file format.

module Opentype.Fileformat
       (-- * Types
         ShortFrac (..), Fixed, FWord, UFWord, GlyphID, WordMap, 
         -- * Main datatype
         OpentypeFont (..), OutlineTables (..), GenericTables,
         -- ** OpentypeFont lenses
         _headTable, _hheaTable, _cmapTable,
         _nameTable, _postTable, _os2Table, _kernTable, _outlineTables, _otherTables,
         _maxpTable, _glyfTable, 
         -- * IO
         readOTFile, writeOTFile,
         -- * Head table
         HeadTable(..),
         -- * Glyf table
         GlyfTable(..), Glyph(..), GlyphOutlines(..), getScaledContours,
         emptyGlyfTable,
         CurvePoint(..), Instructions, GlyphComponent(..),
         -- ** Glyf table lenses
         _glyphContours, _glyphInstructions, _glyphComponents,
         -- * CMap table
         CmapTable(..), CMap(..), PlatformID(..), MapFormat (..),
         emptyCmapTable, 
         -- * Hhea table
         HheaTable(..),
         -- * Maxp table
         MaxpTable(..),
         -- * Name table
         NameTable(..), NameRecord(..),
         -- * Post table
         PostTable(..), PostVersion(..),
         -- * OS/2 table
         OS2Table(..),
         -- * Kern table
         KernTable(..), KernPair(..), _kernPairs
       ) where
import Opentype.Fileformat.Types
import Opentype.Fileformat.Head
import Opentype.Fileformat.Glyph
import Opentype.Fileformat.Hhea
import Opentype.Fileformat.Cmap
import Opentype.Fileformat.Maxp
import Opentype.Fileformat.Name
import Opentype.Fileformat.Post
import Opentype.Fileformat.Kern
import Opentype.Fileformat.OS2
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Data.Maybe
import Data.Bits
import Data.List (zip4, sort)
import Data.Char
import Data.Foldable
import Control.Monad
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.ByteString.Unsafe
import Lens.Micro hiding (strict)
import Lens.Micro.TH
import Lens.Micro.Extras
import qualified Data.Map as M

type GenericTables = M.Map String Lazy.ByteString

-- | truetype or opentype font
data OpentypeFont = OpentypeFont {
  -- | Use apple scaler.  Should not be used for opentype fonts.
  appleScaler :: Bool,
  -- | global information about the font. 
  headTable :: HeadTable,
  -- | global horizontal metrics information
  hheaTable :: HheaTable,
  -- | mapping of character codes to the glyph index values
  cmapTable :: CmapTable,
  -- | information strings in different languages
  nameTable :: NameTable,
  -- | data for postscript printers
  postTable :: PostTable,
  -- | windows specific information
  os2Table :: Maybe OS2Table,
  -- | Kerning tables
  kernTable :: Maybe KernTable,
  -- | tables specific to the outline type (cubic or quadratic).
  outlineTables :: OutlineTables,
  -- | not (yet) supported tables
  otherTables :: GenericTables
  }
  deriving Show


-- | tables for quadratic outlines (truetype or opentype)
data OutlineTables =
  QuadTables MaxpTable GlyfTable |
  CubicTables 
  deriving Show

makeLensesFor [("headTable", "_headTable"),
               ("hheaTable", "_hheaTable"),
               ("cmapTable", "_cmapTable"),
               ("nameTable", "_nameTable"),
               ("postTable", "_postTable"),
               ("outlineTables", "_outlineTables"),
               ("otherTables", "_otherTables")]
               ''OpentypeFont

data ScalerType =
  -- opentype with cff
  CubicScaler |
  -- truetype and opentype with glyf
  QuadScaler |
  -- apple only scaler
  AppleScaler
  deriving Eq
  
type SfntLocs = M.Map Scaler (Word32, Word32)
type Scaler = Word32

_maxpTable :: Traversal' OpentypeFont MaxpTable
_maxpTable f font = case outlineTables font of
  QuadTables m g -> (\m2 -> font { outlineTables = QuadTables m2 g })
                    <$> f m
  _ -> pure font

_glyfTable :: Traversal' OpentypeFont GlyfTable
_glyfTable f font = case outlineTables font of
  QuadTables m g -> (\g2 -> font {outlineTables = QuadTables m g2})
                    <$> f g
  _ -> pure font

_os2Table :: Traversal' OpentypeFont OS2Table
_os2Table f font = case os2Table font of
  Just t -> (\t2 -> font {os2Table = Just t2}) <$> f t
  Nothing -> pure font

_kernTable :: Traversal' OpentypeFont KernTable
_kernTable f font = case kernTable font of
  Just t -> (\t2 -> font {kernTable = Just t2}) <$> f t
  Nothing -> pure font

-- | @getScaledContours scaleOffset glyfTable glyph@: Get the scaled
-- contours for a simple or composite glyph.
getScaledContours :: OpentypeFont -> StandardGlyph -> [[CurvePoint]]
getScaledContours font glyph =
  case preview _glyfTable font of
    Nothing -> []
    Just (GlyfTable vec) ->
      getScaledContours' 10 (appleScaler font) vec glyph

-- | write an opentype font to a file
writeOTFile :: OpentypeFont -> FilePath -> IO ()
writeOTFile font file = 
  case outlineTables font of
    CubicTables ->
      error "cubic splines are not yet supported"
    QuadTables maxpTbl (GlyfTable glyphs) ->
       let (lengths, glyphBs) = runPutM $ writeGlyphs (appleScaler font) glyphs
           (format, locaBs) = runPutM $ writeLoca lengths
           (longHor, hmtxBs) = runPutM $ writeHmtx glyphs
           head2 = updateHead glyphs $
                   (headTable font) {
                     headVersion = 0x00010000,
                     fontDirectionHint = 2,
                     longLocIndices = format }
           hhea2 = updateHhea glyphs $
                   (hheaTable font) {numOfLongHorMetrics = fromIntegral longHor}
           maxp2 = updateMaxp glyphs $
                   maxpTbl {maxpVersion = 0x00010000}
           headBs = Lazy.toStrict $ runPut $ putHeadTable head2
           cmapBs = Lazy.toStrict $ runPut $ putCmapTable $ cmapTable font
           hheaBs = Lazy.toStrict $ runPut $ putHheaTable hhea2
           maxpBs = Lazy.toStrict $ runPut $ putMaxpTable maxp2 
           nameBs = Lazy.toStrict $ runPut $ putNameTable $ nameTable font
           postBs = Lazy.toStrict $ runPut $ putPostTable $ postTable font
           os2Bs  = (Lazy.toStrict . runPut . putOS2Table) <$> os2Table font
           kernBs = (Lazy.toStrict . runPut . putKernTable) <$> kernTable font
           scaler | appleScaler font = AppleScaler 
                  | otherwise = QuadScaler
       in Lazy.writeFile file $ runPut $ 
          writeTables scaler $ concat 
          [[(nameToInt "head", headBs),
            (nameToInt "hhea", hheaBs),
            (nameToInt "maxp", maxpBs)],
           maybeToList $ (nameToInt "OS/2",) <$> os2Bs,
           [(nameToInt "hmtx", Lazy.toStrict hmtxBs),
            (nameToInt "cmap", cmapBs),
            (nameToInt "loca", Lazy.toStrict locaBs),
            (nameToInt "glyf", Lazy.toStrict glyphBs)],
           maybeToList $ (nameToInt "kern",) <$> kernBs,
           [(nameToInt "name", nameBs),
            (nameToInt "post", postBs)]]
  

runGetOrErr :: Get b -> Lazy.ByteString -> Either String b
runGetOrErr g bs = case runGetOrFail g bs of
  Left (_, _, str) -> Left str
  Right (_, _, res) -> Right res

-- | read an opentype font from a file.  
readOTFile :: FilePath -> IO OpentypeFont
readOTFile file = do
  strict <- Strict.readFile file
  let res = do
        (locs, scaler) <- runGetOrErr readTables $
                          Lazy.fromStrict strict
        let readTable tag = case M.lookup (nameToInt tag) locs of
              Nothing -> Left $ "Table " ++ tag ++ " not found."
              Just (offset, _) ->
                Right $ Strict.drop (fromIntegral offset) strict
            readLazy tag = Lazy.fromStrict <$> readTable tag
            readMaybe tag = case M.lookup (nameToInt tag) locs of
              Nothing -> Right Nothing
              Just (offset, _) ->
                Right $ Just $ Lazy.fromStrict $ Strict.drop (fromIntegral offset) strict
        headBs <- runGetOrErr getHeadTable =<< readLazy "head"
        maxpTbl <- runGetOrErr getMaxpTable =<< readLazy "maxp"
        hheaTbl <- runGetOrErr getHheaTable =<< readLazy "hhea"
        offsets <- runGetOrErr (readGlyphLocs (longLocIndices headBs)
                                (fromIntegral $ numGlyphs maxpTbl)) =<<
                   readLazy "loca"
        hmetrics <- runGetOrErr (readHmetrics (fromIntegral $ numOfLongHorMetrics hheaTbl)
                                 (fromIntegral $ numGlyphs maxpTbl))
                    =<< readLazy "hmtx"
        glyphTbl <- readGlyphTable (zip offsets (zipWith (-) offsets (tail offsets)))
                    hmetrics =<< readTable "glyf"
        postTbl <- runGetOrErr getPostTable =<< readLazy "post"
        nameTbl <- readNameTable =<< readTable "name"
        cmapTbl <- readCmapTable =<< readTable "cmap"
        os2tbl <- traverse (runGetOrErr getOS2Table) =<< readMaybe "OS/2"
        kerntbl <- traverse (runGetOrErr getKernTable) =<< readMaybe "kern"
        return $ OpentypeFont (scaler == AppleScaler) headBs hheaTbl
          cmapTbl nameTbl postTbl os2tbl kerntbl
          (QuadTables maxpTbl (GlyfTable glyphTbl)) M.empty
  either (ioError.userError) return res
  
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

checkSum :: Strict.ByteString -> Word32
checkSum bs
  | Strict.length bs < 4 =
      sum [fromIntegral n `shift` l  | (n, l) <- zip (Strict.unpack bs) [24, 16, 8, 0]]
  | otherwise =
      fromIntegral (unsafeIndex bs 0) `shift` 24 +
      fromIntegral (unsafeIndex bs 1) `shift` 16 +
      fromIntegral (unsafeIndex bs 2) `shift` 8 +
      fromIntegral (unsafeIndex bs 3) +
      checkSum (Strict.drop 4 bs)

headWithChecksum :: Strict.ByteString -> Word32 -> Put
headWithChecksum bs cksum = do
  putByteString $ Strict.take 8 bs
  putWord32be $ 0xB1B0AFBA - cksum
  putByteString $ Strict.drop 12 bs
  
putPadding :: Strict.ByteString -> Put
putPadding bs = replicateM_ (pad-sz) (putInt8 0)
  where sz = fromIntegral $ Strict.length bs
        pad = padded sz
  
padded :: (Bits a, Num a) => a -> a
padded len = (len+3) .&. complement 3

writeTables :: ScalerType -> [(Word32, Strict.ByteString)] -> Put
writeTables scaler tables = do
  putByteString unChecked
  let cksumTot = fromIntegral $ sum $ checkSum unChecked:ckSums
  for_ (zip tables tableBs) $
    \((tag,_), bs) -> 
      if tag == nameToInt "head"
        then do headWithChecksum bs cksumTot
                putPadding bs
        else do putByteString bs
                putPadding bs
  where
    entrySelector, searchRange, nTables :: Word16
    nTables = fromIntegral $ length tables
    entrySelector = fromIntegral $ iLog2 nTables
    searchRange = 1 `shift` (fromIntegral entrySelector+4)
    offsets = scanl (+) (fromIntegral $ 16*length tables + 12) (map padded lengths)
    lengths = map Strict.length tableBs
    tableBs = map snd tables
    ckSums = map checkSum tableBs
    unChecked = Lazy.toStrict $ runPut $ do
      putWord32be $ case scaler of
        AppleScaler -> nameToInt "true"
        CubicScaler -> nameToInt "OTTO"
        QuadScaler -> 0x00010000
      putWord16be $ fromIntegral nTables
      putWord16be searchRange
      putWord16be entrySelector
      putWord16be $ nTables * 16 - searchRange
      for_ (sort $ zip4 tables ckSums offsets lengths) $
        \((tag,_), cksum, offset, len) -> do
          putWord32be tag
          putWord32be cksum
          putWord32be $ fromIntegral offset
          putWord32be $ fromIntegral len
