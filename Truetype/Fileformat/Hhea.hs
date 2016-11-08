module Truetype.Fileformat.Hhea
where
import Truetype.Fileformat.Types
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Word
import Control.Monad

data HheaTable = HheaTable {
  -- | 0x00010000 (1.0), will be auto written.
  version :: Fixed,
  -- | Distance from baseline of highest ascender
  ascent :: FWord,
  -- | Distance from baseline of lowest descender
  descent :: FWord,
  -- | typographic line gap
  lineGap :: FWord,
  -- | Will be auto written.
  advanceWidthMax :: UFWord,
  -- | Will be auto written.
  minLeftSideBearing :: FWord,
  -- | Will be auto written.
  minRightSideBearing :: FWord,
  -- | Will be auto written.
  xMaxExtent :: FWord,
  -- | used to calculate the slope of the caret (rise/run) set to 1 for vertical caret
  caretSlopeRise :: Int16,
  -- | 0 for vertical
  caretSlopeRun :: Int16,
  -- | set value to 0 for non-slanted fonts
  caretOffset :: FWord,
  -- | number of advance widths in metrics table. Will be auto written.
  numOfLongHorMetrics :: Word16}

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
  replicateM_ 6 $ putInt16be 0
  putWord16be $ numOfLongHorMetrics table

getHheaTable :: Get HheaTable
getHheaTable =
  HheaTable <$> getWord32be <*> getInt16be <*>
  getInt16be <*> getInt16be <*> getWord16be <*>
  getInt16be <*> getInt16be <*> getInt16be <*>
  getInt16be <*> getInt16be <*> getInt16be <*>
  (getInt16be *> getInt16be *> getInt16be *>
   getInt16be *> getInt16be *> getWord16be)
  
