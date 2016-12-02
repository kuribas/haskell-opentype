{-# LANGUAGE TemplateHaskell #-}
module Opentype.Fileformat.Kern
where
import Opentype.Fileformat.Types
import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Control.Monad
import Data.Foldable
import Lens.Micro.TH

-- | @KernPair left right adjustment@: Pair of kerning values.  left
-- and right are indices in the glyph table.
data KernPair = KernPair Word16 Word16 FWord
  deriving Show

data KernTable = KernTable {
  -- | various flags, will be overwritten with 1 (default)
  coverage :: Word8,
  kernPairs :: [KernPair]}
  deriving Show

makeLensesFor [("kernPairs", "_kernPairs")] ''KernTable

getKernTable :: Get KernTable
getKernTable = do
  version <- getWord16be
  when (version /= 0) $
    fail "Unsupported kern table."
  nTables <- getWord16be
  if nTables == 0
    then return $ KernTable 0 []
    else do
    skip 4
    cov <- getWord16be
    if cov .&. 0xff00 /= 0
      then return $ KernTable 0 []
      else do
      nPairs <- getWord16be
      skip 6
      fmap (KernTable (fromIntegral $ cov .&. 0xff)) $
        replicateM (fromIntegral nPairs) $
        KernPair <$> getWord16be <*>  getWord16be <*> getInt16be

putKernTable :: KernTable -> Put
putKernTable (KernTable _ pairs) = do
  putWord16be 0
  putWord16be 1
  putWord16be 0
  putWord16be $ fromIntegral $ 14+6 * length pairs
  putWord16be $ 1
  putWord16be $ fromIntegral len
  putWord16be searchRange
  putWord16be entrySelector
  putWord16be $ len*6 - searchRange
  for_ pairs $ \(KernPair l r v) -> do
    putWord16be l
    putWord16be r
    putInt16be v
      where
        len = fromIntegral $ length pairs
        entrySelector = fromIntegral $ iLog2 len
        searchRange = 6 * (1 `shift` fromIntegral entrySelector)
    
        


      
      
    
  
  
