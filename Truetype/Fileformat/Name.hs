module Truetype.Fileformat.Name
where
import Truetype.Fileformat.Types
import Data.List (sort)
import Data.Word
import Control.Monad
import Data.Binary.Put
import Data.Binary.Get
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)
import qualified Data.ByteString.Lazy as Lazy

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
data NameTable = NameTable [NameRecord]

data NameRecord = NameRecord {
  namePlatform :: PlatformID,
  nameEncoding :: Word16,
  nameLanguage :: Word16,
  nameID :: Word16,
  nameString :: Lazy.ByteString}

instance Ord NameRecord where
  compare (NameRecord pID eID lang nID _)
    (NameRecord pID2 eID2 lang2 nID2 _) =
    compare (pID, eID, lang, nID) (pID2, eID2, lang2, nID2)

instance Eq NameRecord where
  (NameRecord pID eID lang nID _) ==
    (NameRecord pID2 eID2 lang2 nID2 _) =
    (pID, eID, lang, nID) == (pID2, eID2, lang2, nID2)    


putNameTable (NameTable records_) = do
  putWord16be 0
  putWord16be $ fromIntegral len
  putWord16be $ fromIntegral $ len * 12 + 6
  for_ (zip offsets records) $ \(offset, r) -> do
    putPf $ namePlatform r
    putWord16be $ nameEncoding r
    putWord16be $ nameLanguage r
    putWord16be $ nameID r
    putWord16be $ fromIntegral $ Lazy.length $ nameString r
    putWord16be offset
  traverse_ (putLazyByteString.nameString) records
  return $ len*12 + 6 + fromIntegral (sum lengths)
  where len = length records
        records = sort records_
        lengths = map (fromIntegral . Lazy.length . nameString) records
        offsets = scanl (+) 0 lengths

getNameTable = do
  version <- getWord16be
  when (version > 0) $ fail "Unsupported name table format."
  len <- fromIntegral <$> getWord16be
  skip 2
  (offsets, records) <- fmap unzip $ replicateM len $ do
    pf <- getPf
    enc <- getWord16be
    lang <- getWord16be
    nID <- getWord16be
    len2 <- getWord16be
    offset <- getWord16be
    return (offset, (len2, NameRecord pf enc lang nID Lazy.empty))
  let gaps = zipWith (-) (tail offsets) offsets 
  records2 <- for (zip gaps records) $
    \(gap, (len2, r)) -> do
      str <- getLazyByteString $ fromIntegral len2
      skip (fromIntegral gap-fromIntegral len2)
      return $ r {nameString = str}
  return $ NameTable records2
