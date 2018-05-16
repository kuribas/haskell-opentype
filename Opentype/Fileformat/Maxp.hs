module Opentype.Fileformat.Maxp
where
import Opentype.Fileformat.Types
import Data.Binary.Get
import Data.Binary.Put
import Data.Word


-- | The 'maxp' table establishes the memory requirements for a font.
-- Only instruction data needs to be filled in, since instructions
-- aren't yet supported.  __Note:__ The cff version of this table will
-- be handled automatically.
data MaxpTable = MaxpTable {
  -- | 0x00010000 (1.0).  /Will be overwritten with the correct value./ 
  maxpVersion :: Fixed,
  -- | the number of glyphs in the font.  /Will be overwritten with the correct value./
  numGlyphs :: Word16,
  -- | points in non-compound glyph.  /Will be overwritten with the correct value./
  maxPoints :: Word16,
  -- | contours in non-compound glyph.  /Will be overwritten with the correct value./
  maxContours :: Word16,
  -- | points in compound glyph.  /Will be overwritten with the correct value./
  maxComponentPoints :: Word16,
  -- | contours in compound glyph. /Will be overwritten with the correct value./
  maxComponentContours :: Word16,
  -- | set to 2
  maxZones :: Word16,
  -- | points used in Twilight Zone (Z0)
  maxTwilightPoints :: Word16,
  -- | number of Storage Area locations
  maxStorage :: Word16,
  -- | number of FDEFs
  maxFunctionDefs :: Word16,
  -- | number of IDEFs
  maxInstructionDefs :: Word16,
  -- | maximum stack depth
  maxStackElements :: Word16,
  -- | byte count for glyph instructions
  maxSizeOfInstructions :: Word16,
  -- | number of glyphs referenced at top level. /Will be overwritten with the correct value./
  maxComponentElements :: Word16,
  -- | levels of recursion.  /Will be overwritten with the correct value./
  maxComponentDepth :: Word16}
  deriving Show

emptyMaxpTable :: MaxpTable
emptyMaxpTable = MaxpTable 0x00010000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 

putMaxpTable :: MaxpTable -> Put
putMaxpTable maxp = do
  putWord32be $ maxpVersion maxp
  putWord16be $ numGlyphs maxp
  putWord16be $ maxPoints maxp
  putWord16be $ maxContours maxp
  putWord16be $ maxComponentPoints maxp
  putWord16be $ maxComponentContours maxp
  putWord16be $ maxZones maxp
  putWord16be $ maxTwilightPoints maxp
  putWord16be $ maxStorage maxp
  putWord16be $ maxFunctionDefs maxp
  putWord16be $ maxInstructionDefs maxp
  putWord16be $ maxStackElements maxp
  putWord16be $ maxSizeOfInstructions maxp
  putWord16be $ maxComponentElements maxp
  putWord16be $ maxComponentDepth maxp
  
getMaxpTable :: Get MaxpTable
getMaxpTable =
  MaxpTable <$> getWord32be <*>
  getWord16be <*> getWord16be <*> getWord16be <*>
  getWord16be <*> getWord16be <*> getWord16be <*>
  getWord16be <*> getWord16be <*> getWord16be <*>
  getWord16be <*> getWord16be <*> getWord16be <*>
  getWord16be <*> getWord16be 
  
