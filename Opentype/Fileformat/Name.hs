module Opentype.Fileformat.Name
where
import Opentype.Fileformat.Types
import Data.List (sort, foldl')
import Data.Maybe (fromMaybe)
import Data.Word
import Control.Monad
import Data.Binary.Put
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as Strict

-- | This table allows multilingual strings to be associated with the
-- OpenType™ font file. These strings can represent copyright notices,
-- font names, family names, style names, and so on. To keep this
-- table short, the font manufacturer may wish to make a limited set
-- of entries in some small set of languages; later, the font can be
-- “localized” and the strings translated or added. Other parts of the
-- OpenType font file that require these strings can then refer to
-- them simply by their index number. Clients that need a particular
-- string can look it up by its platform ID, character encoding ID,
-- language ID and name ID. Note that some platforms may require
-- single byte character strings, while others may require double byte
-- strings.
--
--  For historical reasons, some applications which install fonts
-- perform version control using Macintosh platform (platform ID 1)
-- strings from the 'name' table. Because of this, it is strongly
-- recommended that the 'name' table of all fonts include Macintosh
-- platform strings and that the syntax of the version number (name id
-- 5) follows the guidelines given in the opentype specification.
--
-- The encoding for each bytestring depends on the PlatformID and
-- encodingID.  This library doesn't do any conversion.  For more
-- information see the opentype specification:
-- https://www.microsoft.com/typography/otspec/name.htm
data NameTable = NameTable {nameRecords :: [NameRecord]}
  deriving Show

data NameRecord = NameRecord {
  namePlatform :: PlatformID,
  nameEncoding :: Word16,
  nameLanguage :: Word16,
  nameID :: Word16,
  nameString :: Strict.ByteString}
  deriving Show

instance Ord NameRecord where
  compare (NameRecord pID eID lang nID _)
    (NameRecord pID2 eID2 lang2 nID2 _) =
    compare (pID, eID, lang, nID) (pID2, eID2, lang2, nID2)

instance Eq NameRecord where
  (NameRecord pID eID lang nID _) ==
    (NameRecord pID2 eID2 lang2 nID2 _) =
    (pID, eID, lang, nID) == (pID2, eID2, lang2, nID2)    

putNameTable :: NameTable -> Put
putNameTable (NameTable records_) = do
  putWord16be 0
  putWord16be $ fromIntegral len
  putWord16be $ fromIntegral $ len * 12 + 6
  for_ records $ \r -> do
    putPf $ namePlatform r
    putWord16be $ nameEncoding r
    putWord16be $ nameLanguage r
    putWord16be $ nameID r
    putWord16be $ fromIntegral $ Strict.length $ nameString r
    putWord16be $ fromMaybe 0 $ fromIntegral <$>
      HM.lookup (nameString r) offsets
  traverse_ putByteString $ reverse noDups
  where len = length records
        records = sort records_
        (noDups, offsets) = snd $ foldl'
          (\(offset, (noDups2, mp)) r ->
             if HM.member (nameString r) mp
             then (offset, (noDups2, mp))
             else (Strict.length (nameString r) + offset, 
                    (nameString r:noDups2, HM.insert (nameString r) offset mp)))
          (0, ([], HM.empty)) records
        

readNameTable :: Strict.ByteString -> Either String NameTable
readNameTable bs = do
  version <- index16 bs 0
  when (version > 0) $ fail "Unsupported name table format."
  len <- index16 bs 1 
  storage <- index16 bs 2
  records <- for [0..len-1] $ \i -> do
    pf <- toPf =<< index16 bs (3 + i*6)
    enc <- index16 bs $ 3 + i*6 + 1
    lang <- index16 bs $ 3 + i*6 + 2
    nID <- index16 bs $ 3 + i*6 + 3
    len2 <- index16 bs $ 3 + i*6 + 4 
    offset <- index16 bs $ 3 + i*6 + 5
    Right (offset, len2, NameRecord pf enc lang nID)
  records2 <- for records $
    \(offset, len2, r) ->
      if storage+offset+len2 > fromIntegral (Strict.length bs)
        then Left "string storage bounds exceeded"
        else Right $ r (Strict.take (fromIntegral len2) $
                        Strict.drop (fromIntegral $ offset+storage) bs)
  return $ NameTable records2
