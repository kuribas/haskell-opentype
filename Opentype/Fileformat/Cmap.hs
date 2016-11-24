module Opentype.Fileformat.Cmap where
import Opentype.Fileformat.Types
import Data.Binary
import Data.Binary.Put
import Data.List (sort, mapAccumL, foldl')
import Data.Either (either)
import Control.Monad
import Data.Traversable (for)
import Data.Foldable (for_, traverse_)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Bits
import Data.Int
-- import Hexdump -- for debugging

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
  deriving Show

emptyCmapTable :: CmapTable
emptyCmapTable = CmapTable []

data CMap = CMap {
  cmapPlatform :: PlatformID,
  cmapEncoding :: Word16,
  -- | used only in the Macintosh platformID (/DEPRECATED/)
  cmapLanguage :: Word16,
  -- | internal format of the map
  mapFormat :: MapFormat,
  -- | set contains high byte\/word if part of multibyte\/word
  -- character.
  multiByte :: IS.IntSet,
  -- | map from character code to glyph index
  glyphMap :: WordMap GlyphID
  }
  deriving Show

instance Ord CMap where
  compare (CMap pfID encID lang _ _ _) (CMap pfID2 encID2 lang2 _ _ _) =
    compare (pfID, encID, lang) (pfID2, encID2, lang2)

instance Eq CMap where
  (CMap pfID encID lang _ _ _) == (CMap pfID2 encID2 lang2 _ _ _) =
    (pfID, encID, lang) == (pfID2, encID2, lang2)

data MapFormat = 
  -- | 8 bit encoding, contiguous block of bytes.  /LEGACY ONLY./
  MapFormat0 |
  -- | mixed 8\/16 bit encoding with gaps.  /LEGACY ONLY./
  MapFormat2 |
  -- | 16 bit encoding with holes.  This should contain the BMP for a
  -- unicode font.
  MapFormat4 |
  -- | 16 bit single contiguous block (trimmed).
  MapFormat6 |
  -- | mixed 16\/32 bit, for compatibility only, /DO NOT USE/
  MapFormat8 |
  -- | 32 bit single contiguous block (trimmed), for compatibility
  -- only, /DO NOT USE/
  MapFormat10 |
  -- | 32 bit segmented coverage.  This should contain Unicode
  -- encodings with glyphs above 0xFFFF.  It's recommended to save a
  -- subset to format 4, for backwards compatibility.
  MapFormat12
  deriving Show

putCmapTable :: CmapTable -> Put
putCmapTable (CmapTable cmaps_) =
  do putWord16be 0
     putWord16be $ fromIntegral $ length cmaps
     for_ (zip offsets cmaps) $
       \(offset, CMap pfID encID _ _ _ _) ->
         do putPf pfID
            putWord16be encID
            putWord32be offset
     traverse_ putLazyByteString cmapsBs
       where
         cmaps = sort cmaps_
         offsets :: [Word32]
         offsets =
           scanl (+) (fromIntegral $ 4 + 8 * length cmaps) $
           map (fromIntegral . Lazy.length) cmapsBs
         cmapsBs = map (runPut.putCmap) cmaps

readCmapTable :: Strict.ByteString -> Either String CmapTable
readCmapTable bs = do
  version <- index16 bs 0
  when (version /= 0) $
    fail "unsupported cmap version."
  n <- index16 bs 1
  entries <- for [0..n-1] $ \i -> do
    pfID <- toPf =<< (index16 bs $ 2 + i*4)
    encID <- index16 bs $ 2 + i*4 + 1
    offset <- index32 bs $ 2 + fromIntegral i*2
    return (offset, pfID, encID)
  cmaps <- for entries $ \(offset, pfID, encID) -> do
    cm <- readCmap $ Strict.drop (fromIntegral offset) bs
    Right $ cm {cmapPlatform = pfID, cmapEncoding = encID}
  return $ CmapTable cmaps

putCmap :: CMap -> Put
putCmap cmap = case mapFormat cmap of
  MapFormat0 -> putMap0 cmap
  MapFormat2 -> putMap2 cmap
  -- 16 bit encoding with holes
  MapFormat4 -> putMap4 cmap
  -- trimmed 16 bit mapping.
  MapFormat6 -> putMap6 cmap
  -- mixed 16/32 bit encoding (/DEPRECATED/)
  MapFormat8 -> putMap8 cmap
  MapFormat10 -> putMap10 cmap
  -- 32 bit segmented coverage
  MapFormat12 -> putMap12 cmap

readCmap :: Strict.ByteString -> Either String CMap
readCmap bs_ = do
  c <- index16 bs_ 0
  let bs | (c >= 8 && c < 14) = Strict.drop 8 bs_
         | otherwise = Strict.drop 4 bs_
  either fail return $
    case c of 
      0 -> getMap0 bs
      2 -> getMap2 bs
      4 -> getMap4 bs
      6 -> getMap6 bs
      8 -> getMap8 bs
      10 -> getMap10 bs
      12 -> getMap12 bs
      i -> fail $ "unsupported map encoding " ++ show i

subIntMap :: Word32 -> Word32 -> WordMap GlyphID -> WordMap GlyphID
subIntMap from to intMap =
  fst $ M.split (fromIntegral to+1) $ snd $
  M.split (fromIntegral from-1) intMap

asSubtypeFrom :: b -> [(a, b)] -> b
asSubtypeFrom a _ = a

-- Put codes in range, and use zero glyph when no code is found.
putCodes :: Word32 -> Word32 -> [(Word32, GlyphID)] -> Put
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
subCodes :: WordMap GlyphID -> Word32 -> Word32 -> Put
subCodes set start end =
  putCodes start end $ M.toList $ subIntMap start end set

data SubTable2 = SubTable2 {
  highByte :: Word16,
  firstCode :: Word16,
  entryCount :: Word16,
  rangeOffset :: Word16,
  rangeBytes :: Put
  }

putMap0 :: CMap -> PutM ()
putMap0 cmap = do
  putWord16be 0
  putWord16be 262
  putWord16be $ cmapLanguage cmap
  let gm = glyphMap cmap
  for_ [0..255] $ \c ->
    putWord8 $ fromIntegral $
    fromMaybe 0 (M.lookup c gm)

getMap0 :: Strict.ByteString -> Either String CMap
getMap0 bs =
  if Strict.length bs < 258 then
    Left "invalid map format 0"
  else
    do lang <- index16 bs 0
       let gmap = M.fromAscList $
             filter ((/= 0).snd) $
             (flip map) [0..255] $ \c ->
             (fromIntegral c, fromIntegral $ Strict.index bs (c+2))
       Right $ CMap UnicodePlatform 0 lang MapFormat0 IS.empty gmap

putMap2 :: CMap -> PutM ()
putMap2 cmap = do
  putWord16be 2
  putWord16be size
  putWord16be (cmapLanguage cmap)
  putCodes 0 255 $ zip (map (fromIntegral.highByte) subTableCodes) [1::Word16 ..]
  for_ subTables $ \(SubTable2 _ fc ec ro _) ->
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
      subTableCodes =
        filter ((/= 0) . entryCount) $ 
        flip map highBytes $ \hb ->
        let subMap = subIntMap (fromIntegral hb `shift` 8)
                     (fromIntegral hb `shift` 8 .|. 0xff) $
                     glyphMap cmap
            (fstCode, lstCode)
              | M.null subMap = (0, -1)
              | otherwise = (fst $ M.findMin subMap,
                             fst $ M.findMax subMap)
            ec = lstCode - fstCode + 1
            rb = subCodes subMap fstCode lstCode
        in SubTable2 (fromIntegral hb) (fromIntegral fstCode) (fromIntegral ec) 0 rb
          where
      subTables = scanl calcOffset firstTable subTableCodes
      firstTable =
        SubTable2 0 0 256 (fromIntegral $ length subTableCodes * 8 + 2) $
        subCodes (glyphMap cmap) 0 255
      size :: Word16
      size = 518 + 8 * (fromIntegral $ length subTables) + 2 * sum (map entryCount subTables)
      calcOffset prev st = st { rangeOffset = rangeOffset prev - 8 + 2*entryCount prev }
        
getMap2 :: Strict.ByteString -> Either String CMap
getMap2 bs = do
  lang <- index16 bs 0
  highBytes <- do
    l <- traverse (index16 bs) [1..256]
    Right $ map fst $ filter ((/=0).snd) $ zip [0::Int ..255] l
  l <- for [0::Word16 .. fromIntegral $ length highBytes] $ \i -> do
    fstCode <- index16 bs (fromIntegral $ 257 + i*4)
    cnt <- index16 bs (fromIntegral $ 257 + i*4 + 1)
    delta <- index16 bs (fromIntegral $ 257 + i*4 + 2)
    ro <- index16 bs (fromIntegral $ 257 + i*4 + 3)
    for [0 .. fromIntegral cnt-1] $ \entry -> do
      p <- index16 bs (fromIntegral $ 257 + i*4 + 3 + ro `quot` 2 + entry)
      Right (fromIntegral $ fstCode + entry, if p == 0 then 0 else p + delta)
  let im = M.fromAscList $ filter ((/= 0).snd) $ concat l
      is = IS.fromAscList $ map fromIntegral highBytes
  Right $ CMap UnicodePlatform 0 lang MapFormat2 is im      
      
data Segment4 = RangeSegment Word16 Word16 Word16
              | CodeSegment Word16 Word16 [Word16]
              deriving Show

findRange :: Word32 -> Int64 -> [(Word32, Word16)] -> (Word32, [(Word32, Word16)])
findRange nextI _ [] =
  (nextI-1, [])
findRange nextI offset l@((i,c):r)
  | i == nextI && c == fromIntegral (fromIntegral nextI+offset) =
    findRange (nextI+1) offset r
  | otherwise = (nextI-1, l)

findCodes :: Word32 -> [(Word32, Word16)] -> ([GlyphID], [(Word32, Word16)])
findCodes _ [] = ([], [])
findCodes prevI l@((i,c):r)
  -- maximum gap is 4
  | i - prevI > 4 = ([], l)
  | otherwise = (replicate (fromIntegral $ i-prevI-1) 0 ++ c:c2, r2)
  where (c2, r2) = findCodes i r

getSegments :: [(Word32, Word16)] -> [Segment4]
getSegments [] = [RangeSegment 0xffff 1 0]
getSegments l@((start, c):_)
  | fromIntegral end - start >= 4 ||
    lc <= end-start+1 = 
      RangeSegment (fromIntegral start) (fromIntegral end-fromIntegral start+1) c :
      getSegments r
  | otherwise =
      CodeSegment (fromIntegral start) (fromIntegral lc) codes :
      getSegments r2
  where
    lc = fromIntegral $ length codes
    (end, r) = findRange start (fromIntegral c - fromIntegral start) l
    (codes, r2) = findCodes (start-1) l

data Segment4layout = Segment4layout {
  s4endCode :: Word16,
  s4startCode :: Word16,
  s4idDelta :: Word16,
  s4idRangeOffset :: Word16,
  s4glyphIndex :: [GlyphID] }
  deriving Show
  
putMap4 :: CMap -> PutM ()
putMap4 cmap = do
  putWord16be 4
  putWord16be size
  putWord16be (cmapLanguage cmap)
  putWord16be (segCount*2)
  putWord16be searchRange
  putWord16be entrySelector
  putWord16be $ 2*segCount - searchRange
  traverse_ (put.s4endCode) layout
  putWord16be 0
  traverse_ (put.s4startCode) layout
  traverse_ (put.s4idDelta) layout
  traverse_ (put.s4idRangeOffset) layout
  traverse_ (traverse_ put.s4glyphIndex) layout
    where
      size, segCount, searchRange, entrySelector :: Word16
      entrySelector = iLog2 segCount
      searchRange = 1 `shift` (fromIntegral $ entrySelector+1)
      segments = getSegments $ M.toList $ subIntMap 0 0xffff $ glyphMap cmap
      (codeSize, layout) = mapAccumL foldLayout (segCount*2) segments
      foldLayout offset (RangeSegment start len code) =
        (offset-2, Segment4layout (fromIntegral $ start+len-1)
                   (fromIntegral start) (code-(fromIntegral start)) 0 [])
      foldLayout offset (CodeSegment start len codes) =
        (offset+fromIntegral len*2-2,
         Segment4layout (fromIntegral $ start+len-1)
         (fromIntegral start) 0 offset codes)
      size = 8*segCount + codeSize + 16
      segCount = fromIntegral $ length segments

getMap4 :: Strict.ByteString -> Either String CMap
getMap4 bs = do
    lang <- index16 bs 0
    segCount <- (`quot` 2) <$> index16 bs 1
    gmap <- fmap (M.fromAscList . filter ((/= 0).snd) . concat ) $
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
  putWord16be (cmapLanguage cmap)
  putWord16be fCode
  putWord16be eCount
  subCodes (glyphMap cmap) (fromIntegral fCode) (fromIntegral lastCode)
  where
    size, eCount, fCode, lastCode :: Word16
    size = eCount*2 + 10
    eCount = lastCode - fCode + 1
    fCode = fromIntegral $
      min (fromIntegral (maxBound :: Word16)::Word32) $
      fst $ M.findMin (glyphMap cmap)
    lastCode = fromIntegral $
      min (fromIntegral (maxBound :: Word16)::Word32) $
      fst $ M.findMax (glyphMap cmap)
    
getMap6 :: Strict.ByteString -> Either String CMap
getMap6 bs = do
  lang <- index16 bs 0
  fCode <- index16 bs 1
  eCount <- index16 bs 2
  gmap <- fmap (M.fromAscList . filter ((/= 0).snd)) $
          for [0..eCount-1] $ \i -> do
    g <- index16 bs (i+3)
    Right (fromIntegral $ i + fCode, g)
  Right $ CMap UnicodePlatform 0 lang MapFormat6 IS.empty gmap

putPacked :: Int -> [Int] -> Put
putPacked start highBytes
  | start >= 8192 = return ()
  | otherwise = do
      putWord8 $ foldl' (.|.) 0 $ map ((shift 1) . (.&. 7)) bytes
      putPacked (start+1) rest
        where
          (bytes, rest) = span (\b -> b >= (start*8) &&
                                      b < (start+1)*8)
                          highBytes

readPacked :: Strict.ByteString -> [Int]
readPacked bs =
  [i .|. b `shift` 3 |
   (a, b) <- zip (Strict.unpack bs) [0..8191],
   i <- [0..7],
   a .&. (1 `shift` i) /= 0
  ]
      

findRanges :: [(Word32, GlyphID)] -> [(Word32, Word32, GlyphID)]
findRanges [] = []
findRanges l@((i,c):_) = (i, i2, c) : findRanges next
  where (i2, next) = findRange i (fromIntegral c-fromIntegral i) l
  
putMap8 :: CMap -> PutM ()
putMap8 cmap = do
  putWord16be 8
  putWord16be 0
  putWord32be $ fromIntegral size
  putWord32be (fromIntegral $ cmapLanguage cmap)
  putPacked 0 highBytes
  putWord32be $ fromIntegral nGroups
  for_ ranges $ \(start, end, code) -> do
    putWord32be $ fromIntegral start
    putWord32be $ fromIntegral end
    putWord32be $ fromIntegral code
  where
    size = nGroups * 12 + 8208
    highBytes = IS.toList $ multiByte cmap
    ranges = findRanges $ M.toList $ glyphMap cmap
    nGroups = length ranges

getMap8 :: Strict.ByteString -> Either String CMap
getMap8 bs = do
  _ <- index16 bs 0
  lang <- index16 bs 1
  let is = IS.fromAscList $ readPacked (Strict.drop 4 bs)
  nGroups <- index32 bs 2049
  gmap <- fmap (M.fromAscList . concat) $ for [0..nGroups-1] $ \i -> do
    start <- index32 bs (i*3 + 2050)
    end <- index32 bs (i*3 + 2051)
    glyph <- index32 bs (i*3 + 2052)
    return [(fromIntegral c, fromIntegral $ glyph+c-start) | c <- [start .. end]]
  Right $ CMap UnicodePlatform 0 lang MapFormat8 is gmap

getMap10 :: Strict.ByteString -> Either String CMap
getMap10 bs = do
  lang <- index32 bs 0
  fCode <- index32 bs 1
  eCount <- index32 bs 2
  gmap <- fmap (M.fromAscList . filter ((/= 0).snd)) $
          for [0..eCount-1] $ \i -> do
    g <- index16 bs (fromIntegral i+6)
    Right (fromIntegral $ i + fCode, g)
  Right $ CMap UnicodePlatform 0 (fromIntegral lang) MapFormat6 IS.empty gmap


putMap10 :: CMap -> Put
putMap10 cmap = do
  putWord16be 10
  putWord16be 0
  putWord32be size
  putWord32be $ fromIntegral $ cmapLanguage cmap
  putWord32be fCode
  putWord32be eCount
  subCodes (glyphMap cmap) (fromIntegral fCode) (fromIntegral lastCode)
  where
    size, eCount, fCode, lastCode :: Word32
    size = eCount*2 + 20
    eCount = lastCode - fCode  + 1
    fCode = fromIntegral $ fst $ M.findMin $ glyphMap cmap
    lastCode = fromIntegral $ fst $ M.findMax $ glyphMap cmap

putMap12 :: CMap -> PutM ()
putMap12 cmap = do
  putWord16be 12
  putWord16be 0
  putWord32be $ fromIntegral size
  putWord32be (fromIntegral $ cmapLanguage cmap)
  putWord32be $ fromIntegral nGroups
  for_ ranges $ \(start, end, code) -> do
    putWord32be $ fromIntegral start
    putWord32be $ fromIntegral end
    putWord32be $ fromIntegral code
  where
    size = nGroups * 12 + 16
    ranges = findRanges $ M.toList $ glyphMap cmap
    nGroups = length ranges

getMap12 :: Strict.ByteString -> Either String CMap
getMap12 bs = do
  _ <- index16 bs 0
  lang <- index16 bs 1
  nGroups <- index32 bs 1
  gmap <- fmap (M.fromAscList . concat) $ for [0..nGroups-1] $ \i -> do
    start <- index32 bs (i*3 + 2)
    end <- index32 bs (i*3 + 3)
    glyph <- index32 bs (i*3 + 4)
    return [(fromIntegral c, fromIntegral $ glyph+c-start) | c <- [start .. end]]
  Right $ CMap UnicodePlatform 0 lang MapFormat8 IS.empty gmap

