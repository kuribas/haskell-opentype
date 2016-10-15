module Data.Truetype.Cmap where
import Data.Truetype.Types
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.List (sort)
import Control.Monad
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.ByteString.Unsafe
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe

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
  deriving (Ord, Eq)

putPf :: PlatformID -> Put
putPf UnicodePlatform = putWord16be 0
putPf MacintoshPlatform = putWord16be 1
putPf MicrosoftPlatform = putWord16be 3

getPf :: Get PlatformID
getPf = do
  a <- getWord16be
  case a of
    0 -> return UnicodePlatform
    1 -> return MacintoshPlatform
    3 -> return MicrosoftPlatform
    _ -> fail "unknown platformID."

data CMap = CMap {
  platformID :: PlatformID,
  encodingID :: Word16,
  -- | used only in the Macintosh platformID (DEPRECATED)
  macLanguage :: Word16,
  -- | internal format of the map
  mapFormat :: MapFormat,
  -- | set contains first byte\/word if part of multibyte\/word
  -- character.
  multiByte :: IS.IntSet,
  -- | map of character code to glyphID
  glyphMap :: IM.IntMap GlyphID
  }

instance Ord CMap where
  compare (CMap pfID encID lang _ _ _) (CMap pfID2 encID2 lang2 _ _ _) =
    compare (pfID, encID, lang) (pfID2, encID2, lang2)

instance Eq CMap where
  (CMap pfID encID lang _ _ _) == (CMap pfID2 encID2 lang2 _ _ _) =
    (pfID, encID, lang) == (pfID2, encID2, lang2)

newtype CMapIntern = CMapIntern CMap 

instance Binary CMapIntern where
  put (CMapIntern cmap) = case mapFormat cmap of
    MapFormat0 -> putMap0 cmap
    MapFormat2 -> putMap2 cmap
  -- 16 bit encoding with holes
    MapFormat4 -> putMap4 cmap
  -- trimmed 16 bit mapping.
    MapFormat6 -> putMap6 cmap
  -- mixed 16/32 bit encoding (DEPRECATED)
    MapFormat8 -> putMap8 cmap
    MapFormat10 -> putMap10 cmap
    MapFormat11 -> putMap11 cmap
  -- 32 bit segmented coverage
    MapFormat12 -> putMap12 cmap

  get = undefined

putMap0 :: CMap -> PutM ()
putMap0 cmap = do
  putWord16be 0
  putWord16be 262
  putWord16be $ macLanguage cmap
  let gm = glyphMap cmap
  forM_ [0..255] $ \c ->
    putWord8 $ fromIntegral $
    fromMaybe 0 (IM.lookup c gm)

index16 :: Strict.ByteString -> Int -> Either String Word16
index16 bs i
  | Strict.length bs * 2 - 1 < i ||
    i < 0 = Left "Out of Bounds"
  | otherwise = Right $ b1 * 256 + b2
  where
    b1, b2 :: Word16
    b1 = fromIntegral $ unsafeIndex bs (i*2)
    b2 = fromIntegral $ unsafeIndex bs (i*2 + 1)

getMap0 :: PlatformID -> Word16 -> Strict.ByteString -> Either String CMap
getMap0 pfID encID bs =
  if Strict.length bs /= 258 then
    Left "invalid map format 0"
  else
    do lang <- index16 bs 0
       let gmap = IM.fromAscList $
             (flip map) [0..255] $ \c ->
             (fromIntegral c, fromIntegral $ Strict.index bs (c+2))
       Right $ CMap pfID encID lang MapFormat0 IS.empty gmap

subIntMap :: Int -> Int -> IM.IntMap Word16 -> IM.IntMap Word16
subIntMap from to intMap =
  fst $ IM.split (fromIntegral to) $ snd $
  IM.split (fromIntegral from-1) intMap

putCodes :: Int -> Int -> [(Int, Word16)] -> Put
putCodes start end []
  | start < end = do putWord16be 0
                     putCodes (start+1) end []
  | otherwise = putWord16be 0
      
putCodes start end l@((i, code):rest)
  | start < i = do putWord16be 0
                   putCodes (start+1) end l 
  | otherwise = do putWord16be code
                   putCodes (i+1) end rest

subCodes :: IM.IntMap Word16 -> Int -> Int -> Put
subCodes set start end =
  putCodes start end $ IM.toList $ subIntMap start end set

data SubTable2 = SubTable2 {
  firstCode :: Word16,
  entryCount :: Word16,
  rangeOffset :: Word16,
  rangeBytes :: Put
  }

putMap2 :: CMap -> PutM ()
putMap2 cmap = do
  putWord16be 2
  putWord16be len
  putWord16be (macLanguage cmap)
  putCodes 1 255 $ zip highBytes [(1)..]
  forM_ subTables $ \(SubTable2 fc ec ro _) ->
    do putWord16be fc
       putWord16be ec
       putWord16be 0
       putWord16be ro
  forM_ subTables rangeBytes
    where
      highBytes :: [Int]
      highBytes =
        IS.toAscList $ fst $
        IS.split 255 (multiByte cmap)
      subTables = scanl calcSubTable firstTable highBytes
      firstTable =
        SubTable2 0 256 (fromIntegral $ length highBytes * 8 + 2) $
        subCodes (glyphMap cmap) 0 255
      len :: Word16
      len = 512 + 6 + 8 * (fromIntegral $ length highBytes+1) + 2 * sum (map entryCount subTables)
      calcSubTable prev hb =
        SubTable2 (fromIntegral fstCode) (fromIntegral ec) (rangeOffset prev + 2*entryCount prev - 8) rb 
        where
          subMap =
            subIntMap (hb * 256) (hb * 256 + 255) $
            glyphMap cmap
          fstCode = fst $ IM.findMin subMap
          lstCode = fst $ IM.findMax subMap
          ec = lstCode - fstCode + 1
          rb = subCodes subMap fstCode lstCode

getMap2 :: PlatformID -> Word16 -> Strict.ByteString -> Either String CMap
getMap2 pfID encID bs = do
  lang <- index16 bs 0
  highBytes <- do
    l <- mapM (index16 bs) [1..256]
    Right $ map fst $ filter ((/=0).snd) $ zip [0..255] l
  l <- forM (0:highBytes) $ \i -> do
    fstCode <- index16 bs (fromIntegral $ 259 + i*4)
    cnt <- index16 bs (fromIntegral $ 259 + i*4 + 1)
    delta <- index16 bs (fromIntegral $ 259 + i*4 + 2)
    ro <- index16 bs (fromIntegral $ 259 + i*4 + 3)
    forM [0 .. fromIntegral cnt-1] $ \entry -> do
      p <- index16 bs (fromIntegral $ 259 + i*4 - 2 + ro + entry)
      Right (fromIntegral $ fstCode + entry, if p == 0 then 0 else p + delta)
  let im = IM.fromAscList $ filter ((/= 0).snd) $ concat l
      is = IS.fromAscList $ map fromIntegral highBytes
  Right $ CMap pfID encID lang MapFormat2 is im      
      
data Segment4 = SegmentRange Int Int Int
              | SegmentCodes Int Int [Word16]

findRange :: Int -> Int -> Int -> [(Int, Int)] -> (Segment4, [(Int, Int)])
findRange start nextI nextC [] =
  (SegmentRange start (nextI-1) (nextC-1), [])
findRange start nextI nextC ((i,c):r)
  | i == nextI && c == nextC = findRange start (nextI+1) (nextC+1) r
  | otherwise = (SegmentRange start (nextI-1) (nextC-1), r)

findCodes :: Int -> [(Int, Int)] -> ([Int], [(Int, Int)])
findCodes _ [] = ([], [])
findCodes prevI l@((i,c):r)
  -- maximum gap is 4
  | i - prevI > 4 = ([], l)
  | otherwise = (replicate (i-prevI-1) 0 ++ c:c2, r2)
  where (c2, r2) = findCodes i r
          
putMap4 = undefined
putMap6 = undefined
putMap8 = undefined
putMap10 = undefined
putMap11 = undefined
putMap12 = undefined

data MapFormat = 
  -- | legacy 8 bit encoding, contiguous block of the first 256 bytes
  MapFormat0 |
  -- | legacy mixed 8/16 bit encoding 
  MapFormat2 |
  -- | 16 bit encoding with holes.  This should contain the BMP for a
  -- unicode font.
  MapFormat4 |
  -- | 16 bit single contiguous block (trimmed).  Useful For a block
  -- without holes.
  MapFormat6 |
  -- | legacy encoding, mixed 16/32 bit
  MapFormat8 |
  MapFormat10 |
  MapFormat11 |
  -- | 32 bit segmented coverage
  MapFormat12 

instance Binary CmapTable where
  put (CmapTable cmaps_) =
    do putWord16be 0
       putWord16be $ fromIntegral $ length cmaps
       forM_ (zip offsets cmaps) $
         \(len, CMap pfID encID _ _ _ _) ->
         do putPf pfID
            putWord16be encID
            putWord32be len
       mapM_ putLazyByteString cmapsBs 
         where
           cmaps = sort cmaps_
           offsets :: [Word32]
           offsets =
             scanl (+) (fromIntegral $ 8 * length cmaps) $
             map (fromIntegral . Lazy.length) cmapsBs
           cmapsBs = map (encode . CMapIntern) cmaps
    
  get = do
    _ <- getWord16be
    n <- liftM fromIntegral getWord16be
    entries <- replicateM n $ do
      pfID <- getPf :: Get PlatformID
      encID <- getWord16be
      _offset <- getWord32be
      return (pfID, encID)
    cmaps <- forM entries $ \(pfID, encID) -> do
      CMapIntern cm <- get
      return $ cm {platformID = pfID, encodingID = encID}
    return $ CmapTable cmaps
      
    
