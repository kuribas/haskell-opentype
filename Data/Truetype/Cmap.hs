module Data.Truetype.Cmap where
import Data.Truetype.Types
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.List (sort)
import Control.Monad
import qualified Data.ByteString.Lazy as BS
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

instance Binary PlatformID where
  put UnicodePlatform = putWord16be 0
  put MacintoshPlatform = putWord16be 1
  put MicrosoftPlatform = putWord16be 3

  get = do
    a <- getWord16be
    case a of
      0 -> return UnicodePlatform
      1 -> return MacintoshPlatform
      3 -> return MicrosoftPlatform
      _ -> fail "illegal platformID."

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
  -- | 32 bit segmented coverage
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

getMap0 :: PlatformID -> Word16 -> Int -> Get CMapIntern
getMap0 pfID encID size =
  if size /= 262 then
    fail "invalid map format 0"
  else do
    lang <- getWord16be
    gmap <-
      liftM IM.fromAscList $
      forM [0..255] $ \c ->
      do gid <- getWord8
         return (c, fromIntegral gid)
    return $ CMapIntern $
      CMap pfID encID lang MapFormat0 IS.empty gmap
  
putMap2 = undefined
putMap4 = undefined
putMap6 = undefined
putMap8 = undefined
putMap10 = undefined
putMap11 = undefined
putMap12 = undefined

data MapFormat = 
  -- | array of the first 256 glyphs (DEPRECATED)
  MapFormat0 |
  -- | mixed 8/16 bit encoding (DEPRECATED)
  MapFormat2 |
  -- | 16 bit encoding with holes
  MapFormat4 |
  -- | trimmed 16 bit mapping.
  MapFormat6 |
  -- | mixed 16/32 bit encoding (DEPRECATED)
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
         do put pfID
            putWord16be encID
            putWord32be len
       mapM_ putLazyByteString cmapsBs 
         where
           cmaps = sort cmaps_
           offsets :: [Word32]
           offsets =
             scanl (+) (fromIntegral $ 8 * length cmaps) $
             map (fromIntegral . BS.length) cmapsBs
           cmapsBs = map (encode . CMapIntern) cmaps
    
  get = do
    _ <- getWord16be
    n <- liftM fromIntegral getWord16be
    entries <- replicateM n $ do
      pfID <- get :: Get PlatformID
      encID <- getWord16be
      _offset <- getWord32be
      return (pfID, encID)
    cmaps <- forM entries $ \(pfID, encID) -> do
      CMapIntern cm <- get
      return $ cm {platformID = pfID, encodingID = encID}
    return $ CmapTable cmaps
      
    
