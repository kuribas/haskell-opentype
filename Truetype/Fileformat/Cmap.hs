module Truetype.Fileformat.Cmap where
import Truetype.Fileformat.Types
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.List (sort, mapAccumL)
import Data.Either (either)
import Control.Monad
import Data.Traversable (for)
import Data.Foldable (for_)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.ByteString.Unsafe
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Bits
import Hexdump

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

data MapFormat = 
  -- | legacy 8 bit encoding, contiguous block of bytes
  MapFormat0 |
  -- | legacy mixed 8/16 bit encoding 
  MapFormat2 |
  -- | 16 bit encoding with holes.  This should contain the BMP for a
  -- unicode font.
  MapFormat4 |
  -- | 16 bit single contiguous block (trimmed).
  MapFormat6 |
  -- | mixed 16/32 bit, for compatibility only, do not use
  MapFormat8 |
  -- | 32 bit single contiguous block (trimmed), for compatibility
  -- only, do not use
  MapFormat10 |
  
  MapFormat12 |
  -- | 32 bit segmented coverage
  MapFormat13 

instance Binary CmapTable where
  put (CmapTable cmaps_) =
    do putWord16be 0
       putWord16be $ fromIntegral $ length cmaps
       for_ (zip offsets cmaps) $
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
    cmaps <- for entries $ \(pfID, encID) -> do
      CMapIntern cm <- get
      return $ cm {platformID = pfID, encodingID = encID}
    return $ CmapTable cmaps

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
    i -> fail $ "unknown platformID " ++ show i 

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
  -- 32 bit segmented coverage
    MapFormat12 -> putMap12 cmap

  get = do
    c <- getWord16be
    bs <- if (c >= 8 && c < 14)
          then do _ <- getWord16be
                  sz <- fromIntegral <$> getWord32be
                  getByteString (sz - 8)
          else do sz <- fromIntegral <$> getWord16be
                  getByteString (sz - 4)
    either fail (return . CMapIntern) $
      case c of 
        0 -> getMap0 bs
        2 -> getMap2 bs
        4 -> getMap4 bs
        6 -> getMap6 bs
        8 -> getMap8 bs
        10 -> getMap10 bs
        12 -> getMap12 bs
        i -> fail $ "unkown encoding type " ++ show i

index16 :: Strict.ByteString -> Word16 -> Either String Word16
index16 bs i
  | Strict.length bs < fromIntegral ((i+1)*2) ||
    i < 0 = Left $ "Index " ++ show i ++ " out of Bounds"
  | otherwise = Right $ b1 * 256 + b2
  where
    b1, b2 :: Word16
    b1 = fromIntegral $ unsafeIndex bs (fromIntegral $ i*2)
    b2 = fromIntegral $ unsafeIndex bs (fromIntegral $ i*2 + 1)

index32 :: Strict.ByteString -> Word32 -> Either String Word32
index32 bs i
  | Strict.length bs < fromIntegral ((i+1)*4) ||
    i < 0 = Left $ "Index " ++ show i ++ " Out of Bounds"
  | otherwise = Right $ b1 `shift` 24 .&. b2 `shift` 16 .&. b3 `shift` 8 + b4
  where
    b1, b2, b3, b4 :: Word32
    b1 = fromIntegral $ unsafeIndex bs (fromIntegral $ i*4)
    b2 = fromIntegral $ unsafeIndex bs (fromIntegral $ i*4 + 1)
    b3 = fromIntegral $ unsafeIndex bs (fromIntegral $ i*4 + 2)
    b4 = fromIntegral $ unsafeIndex bs (fromIntegral $ i*4 + 3)

subIntMap :: Int -> Int -> IM.IntMap a -> IM.IntMap a
subIntMap from to intMap =
  fst $ IM.split (fromIntegral to+1) $ snd $
  IM.split (fromIntegral from-1) intMap

asSubtypeFrom :: b -> [(a, b)] -> b
asSubtypeFrom a _ = a

-- Put codes in range, and use zero glyph when no code is found.
putCodes :: Int -> Int -> [(Int, GlyphID)] -> Put
putCodes start end _
  | start > end = return ()

putCodes start end [] = do
  putWord16be 0
  putCodes (start+1) end []
      
putCodes start end l@((i, code):rest)
  | start < i = do putWord16be 0
                   putCodes (start+1) end l
  | otherwise = do putWord16be code
                   putCodes (i+1) end rest

-- write subcodes as range, with zero glyph for missing codes.
subCodes :: IM.IntMap Word16 -> Int -> Int -> Put
subCodes set start end =
  putCodes start end $ IM.toList $ subIntMap start end set


data SubTable2 a = SubTable2 {
  firstCode :: a,
  entryCount :: a,
  rangeOffset :: a,
  rangeBytes :: Put
  }

putMap0 :: CMap -> PutM ()
putMap0 cmap = do
  putWord16be 0
  putWord16be 262
  putWord16be $ macLanguage cmap
  let gm = glyphMap cmap
  for_ [0..255] $ \c ->
    putWord8 $ fromIntegral $
    fromMaybe 0 (IM.lookup c gm)

getMap0 :: Strict.ByteString -> Either String CMap
getMap0 bs =
  if Strict.length bs /= 258 then
    Left "invalid map format 0"
  else
    do lang <- index16 bs 0
       let gmap = IM.fromAscList $
             filter ((/= 0).snd) $
             (flip map) [0..255] $ \c ->
             (fromIntegral c, fromIntegral $ Strict.index bs (c+2))
       Right $ CMap UnicodePlatform 0 lang MapFormat0 IS.empty gmap

putMap2 :: CMap -> PutM ()
putMap2 cmap = do
  putWord16be 2
  putWord16be size
  putWord16be (macLanguage cmap)
  putCodes 0 255 $ zip highBytes [1::Word16 ..]
  for_ subTables $ \(SubTable2 fc ec ro _) ->
    do putWord16be fc
       putWord16be ec
       putWord16be 0
       putWord16be ro
  for_ subTables rangeBytes
    where
      highBytes :: [Int]
      highBytes =
        IS.toList $ fst $
        IS.split 255 (multiByte cmap)
      subTables = scanl calcSubTable firstTable highBytes
      firstTable =
        SubTable2 0 256 (fromIntegral $ length highBytes * 8 + 2) $
        subCodes (glyphMap cmap) 0 255
      size :: Word16
      size = 518 + 8 * (fromIntegral $ length highBytes+1) + 2 * sum (map entryCount subTables)
      calcSubTable prev hb =
        SubTable2 (fromIntegral fstCode) (fromIntegral ec)
        (rangeOffset prev - 8 + 2*entryCount prev) rb
        where
          subMap =
            subIntMap (hb `shift` 8) (hb `shift` 8 + 255) $
            glyphMap cmap
          fstCode | IM.null subMap = 0
                  | otherwise = fst $ IM.findMin subMap
          lstCode | IM.null subMap = -1
                  | otherwise = fst $ IM.findMax subMap
          ec = lstCode - fstCode + 1
          rb = subCodes subMap fstCode lstCode

getMap2 :: Strict.ByteString -> Either String CMap
getMap2 bs = do
  lang <- index16 bs 0
  highBytes <- do
    l <- mapM (index16 bs) [1..256]
    Right $ map fst $ filter ((/=0).snd) $ zip [0..255] l
  l <- for [0::Word16 .. fromIntegral $ length highBytes] $ \i -> do
    fstCode <- index16 bs (fromIntegral $ 257 + i*4)
    cnt <- index16 bs (fromIntegral $ 257 + i*4 + 1)
    delta <- index16 bs (fromIntegral $ 257 + i*4 + 2)
    ro <- index16 bs (fromIntegral $ 257 + i*4 + 3)
    for [0 .. fromIntegral cnt-1] $ \entry -> do
      p <- index16 bs (fromIntegral $ 257 + i*4 + 3 + ro `quot` 2 + entry)
      Right (fromIntegral $ fstCode + entry, if p == 0 then 0 else p + delta)
  let im = IM.fromAscList $ filter ((/= 0).snd) $ concat l
      is = IS.fromAscList $ map fromIntegral highBytes
  Right $ CMap UnicodePlatform 0 lang MapFormat2 is im      
      
data Segment4 = RangeSegment Int Int Word16
              | CodeSegment Int Int [Word16]
              deriving Show

findRange :: Int -> Int -> [(Int, Word16)] -> (Int, [(Int, Word16)])
findRange nextI _ [] =
  (nextI-1, [])
findRange nextI offset l@((i,c):r)
  | i == nextI && fromIntegral c == nextI+offset = findRange (nextI+1) offset r
  | otherwise = (nextI-1, l)

findCodes :: Int -> [(Int, Word16)] -> ([GlyphID], [(Int, Word16)])
findCodes _ [] = ([], [])
findCodes prevI l@((i,c):r)
  -- maximum gap is 4
  | i - prevI > 4 = ([], l)
  | otherwise = (replicate (i-prevI-1) 0 ++ c:c2, r2)
  where (c2, r2) = findCodes i r

getSegments :: [(Int, Word16)] -> [Segment4]
getSegments [] = [RangeSegment 0xffff 1 0]
getSegments l@((start, c):_)
  | end - start >= 4 ||
    lc <= end-start+1 = 
      RangeSegment start (end-start+1) (c+fromIntegral (end-start)) :
      getSegments r
  | otherwise =
      CodeSegment start lc codes :
      getSegments r2
  where
    lc = length codes
    (end, r) = findRange start (fromIntegral c - start) l
    (codes, r2) = findCodes (start-1) l

data Segment4layout = Segment4layout {
  s4endCode :: Word16,
  s4startCode :: Word16,
  s4idDelta :: Word16,
  s4idRangeOffset :: Word16,
  s4glyphIndex :: [GlyphID] }
  
putMap4 :: CMap -> PutM ()
putMap4 cmap = do
  putWord16be 4
  putWord16be size
  putWord16be (macLanguage cmap)
  putWord16be (segCount*2)
  putWord16be searchRange
  putWord16be entrySelector
  putWord16be $ 2*segCount - searchRange
  mapM_ (put.s4endCode) layout
  putWord16be 0
  mapM_ (put.s4startCode) layout
  mapM_ (put.s4idDelta) layout
  mapM_ (put.s4idRangeOffset) layout
  mapM_ (mapM_ put.s4glyphIndex) layout
    where
      size, segCount, searchRange, entrySelector :: Word16
      entrySelector = iLog2 segCount
      searchRange = 1 `shift` (fromIntegral $ entrySelector+1)
      segments = getSegments $ IM.toList $ glyphMap cmap
      (codeSize, layout) = mapAccumL foldLayout (segCount*2) segments
      foldLayout offset (RangeSegment start len code) =
        (offset-2, Segment4layout (fromIntegral $ start+len-1) (fromIntegral start) (code-(fromIntegral start)) 0 [])
      foldLayout offset (CodeSegment start len codes) =
        (offset+fromIntegral len*2-2, Segment4layout (fromIntegral $ start+len-1) (fromIntegral start) 0 offset codes)
      size = 8*segCount + codeSize + 16
      segCount = fromIntegral $ length segments

getMap4 :: Strict.ByteString -> Either String CMap
getMap4 bs = do
    lang <- index16 bs 0
    segCount <- (`quot` 2) <$> index16 bs 1
    gmap <- fmap (IM.fromAscList . filter ((/= 0).snd) . concat ) $
            for [0::Word16 .. segCount-2] $ \i ->
      do idDelta <- index16 bs (i + 6 + segCount*2)
         endCode <- index16 bs (i + 5)
         startCode <- index16 bs (i + 6 + segCount)
         idRangeOffset <- index16 bs (i + 6 + segCount*3)
         if idRangeOffset == 0
           then Right [(fromIntegral c, c+idDelta) | c <- [startCode .. endCode]]
           else for [0..endCode-startCode] $ \j ->
           do glyph <- index16 bs (fromIntegral $ i + 6 + segCount*3 + idRangeOffset`div`2 + j)
              Right (fromIntegral $ startCode + j, glyph)
    Right $ CMap UnicodePlatform 0 lang MapFormat4 IS.empty gmap

putMap6 :: CMap -> PutM ()
putMap6 cmap = do
  putWord16be 6
  putWord16be size
  putWord16be (macLanguage cmap)
  putWord16be fCode
  putWord16be eCount
  subCodes (glyphMap cmap) (fromIntegral fCode) (fromIntegral lastCode)
  where
    size, eCount, fCode, lastCode :: Word16
    size = eCount*2 + 10
    eCount = lastCode - fCode + 1
    fCode = fromIntegral $
      min (fromIntegral (maxBound :: Word16)::Int) $
      fst $ IM.findMin (glyphMap cmap)
    lastCode = fromIntegral $
      min (fromIntegral (maxBound :: Word16)::Int) $
      fst $ IM.findMax (glyphMap cmap)
    
getMap6 :: Strict.ByteString -> Either String CMap
getMap6 bs = do
  lang <- index16 bs 0
  fCode <- index16 bs 1
  eCount <- index16 bs 2
  gmap <- fmap (IM.fromAscList . filter ((/= 0).snd)) $
          for [0..eCount-1] $ \i -> do
    g <- index16 bs (i+3)
    Right (fromIntegral $ i + fCode, g)
  Right $ CMap UnicodePlatform 0 lang MapFormat6 IS.empty gmap

putPacked :: Int -> [Int] -> Put
putPacked start highBytes
  | start >= 8192 = return ()
  | otherwise = do
      putWord8 $ sum $ map ((shift 1) . subtract (start `shift` 8)) bytes
      putPacked (start+1) rest
        where
          (bytes, rest) = span (<(start+1) `shift` 8) highBytes

readPacked :: Int -> Strict.ByteString -> [Int]
readPacked start bs =
  [1`shift` i .&. b `shift` 8 |
   (a, b) <- zip (Strict.unpack bs) [0..8191],
   i <- [1..8],
   a .&. (1 `shift` i) /= 0
  ]
      

findRanges :: [(Int, GlyphID)] -> [(Int, Int, GlyphID)]
findRanges [] = []
findRanges l@((i,c):_) = (i, i2, c) : findRanges next
  where (i2, next) = findRange i (fromIntegral c-i) l
  
putMap8 :: CMap -> PutM ()
putMap8 cmap = do
  putWord16be 8
  putWord16be 0
  putWord32be $ fromIntegral size
  putWord32be (fromIntegral $ macLanguage cmap)
  putPacked 0 highBytes
  putWord32be $ fromIntegral nGroups
  for_ ranges $ \(start, end, code) -> do
    putWord32be $ fromIntegral start
    putWord32be $ fromIntegral end
    putWord32be $ fromIntegral code
  where
    size = nGroups * 16 + 8196
    highBytes = IS.toList $ multiByte cmap
    ranges = findRanges $ IM.toList $ glyphMap cmap
    nGroups = length ranges

getMap8 :: Strict.ByteString -> Either String CMap
getMap8 bs = do
  _ <- index16 bs 0
  lang <- index16 bs 1
  let is = IS.fromAscList $ readPacked 0 bs
  nGroups <- index32 bs 2049
  gmap <- fmap (IM.fromAscList . concat) $ for [1..nGroups] $ \i -> do
    start <- index32 bs (i*3 + 2049)
    end <- index32 bs (i*3 + 2050)
    glyph <- index32 bs (i*3 + 2051)
    return [(fromIntegral c, fromIntegral $ glyph+c-start) | c <- [start .. end]]
  Right $ CMap UnicodePlatform 0 lang MapFormat8 is gmap

getMap10 :: Strict.ByteString -> Either String CMap
getMap10 bs = do
  lang <- index32 bs 0
  fCode <- index32 bs 1
  eCount <- index32 bs 2
  gmap <- fmap (IM.fromAscList . filter ((/= 0).snd)) $
          for [1..eCount] $ \i -> do
    g <- index16 bs (fromIntegral i+6)
    Right (fromIntegral $ i + fCode, g)
  Right $ CMap UnicodePlatform 0 (fromIntegral lang) MapFormat6 IS.empty gmap


putMap10 :: CMap -> Put
putMap10 cmap = do
  putWord16be 10
  putWord16be 0
  putWord32be size
  putWord32be $ fromIntegral $ macLanguage cmap
  putWord32be fCode
  putWord32be eCount
  subCodes (glyphMap cmap) (fromIntegral fCode) (fromIntegral lastCode)
  where
    size, eCount, fCode, lastCode :: Word32
    size = eCount*2 + 28
    eCount = lastCode - fCode
    fCode = fromIntegral $ fst $ IM.findMin $ glyphMap cmap
    lastCode = fromIntegral $ fst $ IM.findMax $ glyphMap cmap

putMap12 :: CMap -> PutM ()
putMap12 cmap = do
  putWord16be 12
  putWord16be 0
  putWord32be $ fromIntegral size
  putWord32be (fromIntegral $ macLanguage cmap)
  putWord32be $ fromIntegral nGroups
  for_ ranges $ \(start, end, code) -> do
    putWord32be $ fromIntegral start
    putWord32be $ fromIntegral end
    putWord32be $ fromIntegral code
  where
    size = nGroups * 12 + 16
    ranges = findRanges $ IM.toList $ glyphMap cmap
    nGroups = length ranges

getMap12 :: Strict.ByteString -> Either String CMap
getMap12 bs = do
  _ <- index16 bs 0
  lang <- index16 bs 1
  nGroups <- index32 bs 1
  gmap <- fmap (IM.fromAscList . concat) $ for [0..nGroups-1] $ \i -> do
    start <- index32 bs (i*3 + 2)
    end <- index32 bs (i*3 + 3)
    glyph <- index32 bs (i*3 + 4)
    return [(fromIntegral c, fromIntegral $ glyph+c-start) | c <- [start .. end]]
  Right $ CMap UnicodePlatform 0 lang MapFormat8 IS.empty gmap

createMap l s f = encode $ CMapIntern $ CMap UnicodePlatform 0 1 f (IS.fromList s) (IM.fromList l)
printMap l s f = putStrLn $ prettyHex $ Lazy.toStrict $ createMap l s f
getMap l s f = (is, im)
  where
    CMapIntern (CMap _ _ _ _ is im) = decode $ createMap l s f
