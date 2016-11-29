module Opentype.Fileformat.Hhea
where
import Opentype.Fileformat.Types
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Word
import Control.Monad

-- | This table contains information for horizontal layout. 
data HheaTable = HheaTable {
  -- | 0x00010000 (1.0), will be overwritten.
  version :: Fixed,
  -- | Distance from baseline of highest ascender.  /Will be overwritten./
  ascent :: FWord,
  -- | Distance from baseline of lowest descender.  /Will be overwritten./
  descent :: FWord,
  -- | typographic line gap.  Will be overwritten when OS/2 table is present.
  lineGap :: FWord,
  -- | /Will be overwritten./
  advanceWidthMax :: UFWord,
  -- | /Will be overwritten./
  minLeftSideBearing :: FWord,
  -- | /Will be overwritten./
  minRightSideBearing :: FWord,
  -- | /Will be overwritten./
  xMaxExtent :: FWord,
  -- | used to calculate the slope of the caret (rise/run) set to 1 for vertical caret
  caretSlopeRise :: Int16,
  -- | 0 for vertical
  caretSlopeRun :: Int16,
  -- | set value to 0 for non-slanted fonts
  caretOffset :: FWord,
  -- | number of advance widths in metrics table. /Will be overwritten./
  numOfLongHorMetrics :: Word16}
  deriving Show

putHheaTable :: HheaTable -> Put
putHheaTable table = do
  putWord32be 0x00010000
  putInt16be $ ascent table
  putInt16be $ descent table
  putInt16be $ lineGap table
  putWord16be $ advanceWidthMax table
  putInt16be $ minLeftSideBearing table
  putInt16be $ minRightSideBearing table
  putInt16be $ xMaxExtent table
  putInt16be $ caretSlopeRise table
  putInt16be $ caretSlopeRun table
  putInt16be $ caretOffset table
  replicateM_ 5 $ putInt16be 0
  putWord16be $ numOfLongHorMetrics table
  
getHheaTable :: Get HheaTable
getHheaTable =
  HheaTable <$> getWord32be <*> getInt16be <*>
  getInt16be <*> getInt16be <*> getWord16be <*>
  getInt16be <*> getInt16be <*> getInt16be <*>
  getInt16be <*> getInt16be <*> getInt16be <*>
  (skip 10 *> getWord16be)
  
