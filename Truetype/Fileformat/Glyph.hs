{-# LANGUAGE MultiWayIf, TupleSections #-}
module Truetype.Fileformat.Glyph where
import Truetype.Fileformat.Types
import Truetype.Fileformat.Maxp
import Truetype.Fileformat.Hhea
import Truetype.Fileformat.Head
import qualified Data.Vector as V
import Data.Foldable (traverse_, for_)
import Control.Monad
import Data.Function (fix)
import qualified Data.ByteString.Lazy as Lazy
import Data.Word
import Data.Int
import Data.Maybe (isJust)
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get


-- | This table contains the data that defines the appearance of
-- the glyphs in the font. This includes specification of the points
-- that describe the contours that make up a glyph outline and the
-- instructions that grid-fit that glyph. The glyf table supports the
-- definition of simple glyphs and compound glyphs, that is, glyphs
-- that are made up of other glyphs.
data GlyfTable = GlyfTable (V.Vector Glyph)

data Glyph = Glyph {
  advanceWidth :: Word16,
  leftSideBearing :: Int16,
  glyphXmin :: FWord,
  glyphYmin :: FWord,
  glyphXmax :: FWord,
  glyphYmax :: FWord,
  glyphOutlines :: GlyphOutlines}
  deriving Show

data GlyphOutlines =
  GlyphContours [[CurvePoint]] Instructions |
  CompositeGlyph [GlyphComponent]
  deriving Show

-- | @CurvePoint x y onCurve@: Points used to describe the outline
-- using lines and quadratic beziers.  Coordinates are relative to the
-- previous point.  If two off-curve points follow each other, an
-- on-curve point is added halfway between.
data CurvePoint = CurvePoint FWord FWord Bool
  deriving Show

-- | TODO: make a proper datatype for instructions.
type Instructions = V.Vector Word8

data GlyphComponent =
  GlyphComponent {
  componentID :: Int,
  componentInstructions :: Maybe Instructions,
  -- | transformation matrix for scaling the glyph
  componentXX :: ShortFrac,
  -- | transformation matrix for scaling the glyph
  componentXY :: ShortFrac,
  -- | transformation matrix for scaling the glyph
  componentYX :: ShortFrac,
  -- | transformation matrix for scaling the glyph
  componentYY :: ShortFrac,
  -- | if `matchPoints` is `True`, index of matching point in compound
  -- being constructed, otherwise x shift.  If `scaledComponentOffset`
  -- is `False`, this offset is unscaled (microsoft and opentype
  -- default) in the rasterizer.
  componentX :: Int,
  -- | if `matchPoints` is `True`, index of matching point in compound,
  -- otherwise y shift.  If `scaledComponentOffset` is `False`, this
  -- offset is unscaled (microsoft and opentype default) in the
  -- rasterizer.
  componentY :: Int,
    -- | see previous
  matchPoints :: Bool,
  -- | For the xy values if `matchPoints` is `False`.
  roundXYtoGrid :: Bool,
  -- | Use metrics from this component for the compound glyph.
  useMyMetrics :: Bool, 
  -- | If set, the components of the compound glyph overlap. Use of
  -- this flag is not required in OpenType — that is, it is valid to
  -- have components overlap without having this flag set. It may
  -- affect behaviors in some platforms, however. (See Apple’s
  -- specification for details regarding behavior in Apple platforms.)
  overlapCompound :: Bool,
  -- | The component offset is scaled by the rasterizer (apple).  Should be
  -- set to `False` (opentype default), unless the font is meant to work
  -- with old apple software.
  scaledComponentOffset :: Bool
  }
  deriving Show

readHmetrics :: Int -> Int -> Get [(Word16, Int16)]
readHmetrics 1 m = do
  aw <- getWord16be
  lsb <- getInt16be
  ((aw, lsb):) <$> replicateM (m-1) ((aw,) <$> getInt16be)

readHmetrics 0 _ = fail "no horizontal metrics found"
readHmetrics n m = do
  aw <- getWord16be
  lsb <- getInt16be
  ((aw, lsb):) <$> readHmetrics (n-1) (m-1)

readGlyphSizes :: Bool -> Int -> Get [Int]
readGlyphSizes long n = do
  offsets <- replicateM (n+1) $ 
             if long then fromIntegral <$> getWord32be
             else (*2).fromIntegral <$> getWord16be
  return $ zipWith (-) (tail offsets) offsets
  
readGlyphTable :: [Int] -> [(Word16, Int16)] -> Lazy.ByteString
               -> Either (Lazy.ByteString, ByteOffset, String) (V.Vector Glyph)
readGlyphTable glyphSizes hmetrics glyfBs = do
  glyphs <- getWithBounds glyfBs glyphSizes getGlyph
  return $ V.fromList $ zipWith
    (\g (aw, lsb) -> g {advanceWidth = aw, leftSideBearing = lsb})
    glyphs hmetrics

-- return bytestring lengths
writeGlyphs :: V.Vector Glyph -> PutM (V.Vector Int)
writeGlyphs = traverse writeGlyph

writeGlyph :: Glyph -> PutM Int
writeGlyph g = do
  putLazyByteString bs
  replicateM_ pad (putWord8 0)
  return $ fromIntegral (len+pad)
    where bs = runPut $ putGlyph g
          len = fromIntegral $ Lazy.length bs
          pad = (- fromIntegral len) .&. 3
  
-- return long or short format  
writeLoca :: V.Vector Int -> (Bool, PutM Int)
writeLoca i
  | V.last offsets > 0xffff =
    (True,
     do traverse_ (putWord32be . fromIntegral) offsets
        return $ V.length offsets * 4)
  | otherwise =
    (False,
     do traverse_ (putWord16be . (`quot` 2) . fromIntegral) offsets
        return $ V.length offsets * 2)
  where
    offsets = V.scanl (+) 0 i
    
writeHmtx :: V.Vector Glyph -> (Int, PutM Int)
writeHmtx gs
  | V.null gs = (0, return 0)
  | otherwise = 
    (tl,
     do traverse_ (\g -> do putWord16be (advanceWidth g)
                            putInt16be (leftSideBearing g)
                  ) dbl
        traverse_ (putInt16be.leftSideBearing) sngl
        return (len*4 - tl*2)
    ) where
      findTail i cnt
        | i < 0 = cnt
        | advanceWidth (V.unsafeIndex gs i) == aw =
            findTail (i-1) (cnt+1)
        | otherwise = cnt
      aw = advanceWidth (V.unsafeLast gs)
      len = V.length gs
      tl = findTail (len-1) 0
      (dbl, sngl) = V.splitAt (len-tl) gs
  
putGlyph :: Glyph -> Put
putGlyph (Glyph _ _ xmin ymin xmax ymax outlines) = do
  putInt16be $ case outlines of
    GlyphContours pts _ -> fromIntegral $ length pts
    _ -> -1
  putInt16be xmin
  putInt16be ymin
  putInt16be xmax
  putInt16be ymax
  case outlines of
    GlyphContours pts instrs ->
      putContour pts instrs
    CompositeGlyph comps -> do
      traverse_ (putComponent True) (init comps)
      putComponent False $ last comps

getGlyph :: Get Glyph
getGlyph = do
  n <- getInt16be
  xmin <- getInt16be
  ymin <- getInt16be
  xmax <- getInt16be
  ymax <- getInt16be
  outlines <-
    if n >= 0
    then getContour (fromIntegral n)
    else fmap CompositeGlyph $ fix $ \nextComponent -> do
      (c, more) <- getComponent
      if more then (c:) <$> nextComponent
        else return [c]
  return $ Glyph 0 0 xmin ymin xmax ymax outlines

isShort :: FWord -> Bool
isShort n = abs n <= 255

putCompressFlags :: [Word8] -> Put
putCompressFlags [] = return ()
putCompressFlags (a:r) =
  do if null as
       then putWord8 a
       else do putWord8 (a .|. 8)
               putWord8 $ fromIntegral $ length as + 1
     putCompressFlags r2
       where
         (as, r2) = span (== a) r
         
contourFlags :: [CurvePoint] -> [Word8]
contourFlags [] = []
contourFlags cps@(CurvePoint x y oc: cps2) =
  firstFlag : zipWith pairFlags cps cps2
  where
    firstFlag = fromIntegral $
                makeFlag [oc, isShort x, isShort y, False, 
                          isShort x && x >= 0,
                          isShort y && y >= 0]

pairFlags :: CurvePoint -> CurvePoint -> Word8  
pairFlags (CurvePoint x1 y1 _) (CurvePoint x2 y2 oc) =
  fromIntegral $ 
  makeFlag [oc, sx && not eqX, sy && not eqY, False,
            eqX || (sx && x2 >= 0),
            eqY || (sy && y2 >= 0)]
  where
    sx = isShort x2
    sy = isShort y2
    eqX = x1 == x2
    eqY = y1 == y2

putCoordX :: CurvePoint -> Word8 -> Put
putCoordX (CurvePoint x _ _) flag
  | byteAt flag 1 = putWord8 (fromIntegral $ abs x)
  | byteAt flag 4 = return ()
  | otherwise = putInt16be (fromIntegral x)
  
putCoordY :: CurvePoint -> Word8 -> Put
putCoordY (CurvePoint _ y _) flag
  | byteAt flag 2 = putWord8 (fromIntegral $ abs y)
  | byteAt flag 5 = return ()
  | otherwise = putInt16be (fromIntegral y)

putContour :: [[CurvePoint]] -> V.Vector Word8 -> Put    
putContour points instr = do
  traverse_ (putWord16be.fromIntegral) endPts
  putWord16be $ fromIntegral $ V.length instr
  traverse_ putWord8 instr
  putCompressFlags flags
  zipWithM_ putCoordX allPts flags
  zipWithM_ putCoordY allPts flags
  where
    endPts = tail $ scanl (+) (-1) $ map length points
    allPts = concat points
    flags = contourFlags allPts

getFlags :: Int -> Get [Word8]
getFlags n
  | n <= 0 = return []
  | otherwise = do
      flag <- getWord8
      if flag .&. 8 /= 0
        then do m <- fromIntegral <$> getWord8
                (replicate (min m n) flag ++) <$> getFlags (n-m)
        else (flag:) <$> getFlags (n-1)

getXcoords, getYcoords :: [Word8] -> FWord -> Get [FWord]
getXcoords [] _ = return []
getXcoords (f:r) prev 
  | byteAt f 1 = do
      x <- getWord8
      let x' | byteAt f 4 = fromIntegral x
             | otherwise = - fromIntegral x
      (x':) <$> getXcoords r x'
  | byteAt f 4 = (prev:) <$> getXcoords r prev
  | otherwise = do
      x <- fromIntegral <$> getWord16be
      (x:) <$> getXcoords r x

getYcoords [] _ = return []
getYcoords (f:r) prev 
  | byteAt f 2 = do
      y <- getWord8
      let y' | byteAt f 5 = fromIntegral y
             | otherwise = - fromIntegral y
      (y':) <$> getYcoords r y'
  | byteAt f 5 = (prev:) <$> getYcoords r prev
  | otherwise = do
      y <- fromIntegral <$> getWord16be
      (y:) <$> getYcoords r y

getPoint :: FWord -> FWord -> Word8 -> CurvePoint
getPoint x y flag = CurvePoint x y (byteAt flag 0)

reGroup :: [a] -> [Int] -> [[a]]
reGroup _ []  = []
reGroup l (n:ns) = c : reGroup r ns
  where
    (c, r) = splitAt n l
  
getContour :: Int -> Get GlyphOutlines
getContour 0 =  return $ GlyphContours [] V.empty
getContour nContours = do
  lastPts <- replicateM nContours (fromIntegral <$> getWord16be)
  iLen <- fromIntegral <$> getWord16be
  instructions <- V.replicateM iLen getWord8
  flags <- getFlags $ last lastPts + 1
  xCoords <- getXcoords flags 0
  yCoords <- getYcoords flags 0
  let coords = zipWith3 getPoint xCoords yCoords flags
      contours = reGroup coords $
                 zipWith (-) lastPts ((-1):lastPts)
  return $ GlyphContours contours instructions

isShortInt :: Int -> Bool
isShortInt x = x <= 127 && x >= -128

glyphExtent, glyphRsb :: Glyph -> Int16
glyphRsb g =
  fromIntegral ((fromIntegral (advanceWidth g)  - fromIntegral (leftSideBearing g - (glyphXmax g - glyphXmin g))) :: Int)
glyphExtent glyf = leftSideBearing glyf + (glyphXmax glyf - glyphXmin glyf)

updateHhea :: HheaTable -> V.Vector Glyph -> HheaTable
updateHhea = V.foldl updateHhea1

updateMinMax :: (FWord, FWord, FWord, FWord)
             -> Glyph -> (FWord, FWord, FWord, FWord)
updateMinMax (xmin, ymin, xmax, ymax) g =
  (min xmin (glyphXmin g),
   min ymin (glyphYmin g),
   max xmax (glyphXmax g),
   max ymax (glyphYmax g))

updateHead :: HeadTable -> V.Vector Glyph -> HeadTable
updateHead headTbl vec =
  headTbl {xMin = xmin, yMin = ymin,
           xMax = xmax, yMax = ymax}
  where (xmin, ymin, xmax, ymax) =
          V.foldl updateMinMax (maxBound, maxBound, minBound, minBound) vec

updateHhea1 :: HheaTable -> Glyph -> HheaTable
updateHhea1 hhea g =
  hhea {advanceWidthMax     = max (advanceWidthMax hhea)
                              (advanceWidth g),
        minLeftSideBearing  = min (minLeftSideBearing hhea)
                              (leftSideBearing g),
        minRightSideBearing = min (minRightSideBearing hhea)
                              (glyphRsb g),
        xMaxExtent          = max (xMaxExtent hhea)
                              (glyphExtent g)}
updateMaxp :: V.Vector Glyph -> MaxpTable -> V.Vector Glyph -> MaxpTable
updateMaxp vec = V.foldl (updateMaxp1 vec)

updateMaxp1 :: V.Vector Glyph -> MaxpTable -> Glyph -> MaxpTable
updateMaxp1 vec maxp glyf =
  maxp {numGlyphs            = numGlyphs maxp + 1,
        maxPoints            = max (maxPoints maxp) $
                               glyfPoints vec glyf,
        maxContours          = max (maxContours maxp) $
                               glyfContours vec glyf,
        maxComponentPoints   = max (maxComponentPoints maxp) $
                               componentPoints vec glyf,
        maxComponentContours = max (maxComponentContours maxp) $
                               componentContours vec glyf,
        maxComponentElements = max (maxComponentElements maxp) $
                               componentRefs vec glyf,
        maxComponentDepth    = max (maxComponentDepth maxp) $
                               componentDepth vec glyf}

overComponents :: ([[CurvePoint]] -> Word16) -> ([Word16] ->  Word16)
               -> Int -> V.Vector Glyph -> Glyph -> Word16
overComponents f h maxD v g
  | maxD <= 0 = 0
  | otherwise =
    case glyphOutlines g of
      GlyphContours p _ -> f p
      CompositeGlyph comps ->
        h $ map overSub comps
        where overSub comp = case v V.!? componentID comp of
                Nothing -> 0
                Just g2 -> overComponents f h (maxD-1) v g2
  
glyfPoints, glyfContours, componentRefs, componentDepth, componentPoints, componentContours :: V.Vector Glyph -> Glyph -> Word16

glyfPoints  =
  overComponents (sum . map (fromIntegral.length)) (const 0) 2

glyfContours =
  overComponents (fromIntegral.length) (const 0) 2

componentRefs =
  overComponents (const 0) (fromIntegral.length) 2

componentPoints =
  overComponents (sum . map (fromIntegral . length)) sum 10

componentDepth =
  overComponents (const 0) ((+1).maximum) 10

componentContours =
  overComponents (fromIntegral.length) sum 10

putComponent :: Bool -> GlyphComponent -> Put
putComponent more c = do
  putWord16be flag
  putWord16be $ fromIntegral $ componentID c
  case (byteAt flag 0, byteAt flag 1) of
    (False, False) -> do
      putWord8 $ fromIntegral $ componentX c
      putWord8 $ fromIntegral $ componentY c
    (False, True) -> do
      putInt8 $ fromIntegral $ componentX c
      putInt8 $ fromIntegral $ componentY c
    (True, False) -> do
      putWord16be $ fromIntegral $ componentX c
      putWord16be $ fromIntegral $ componentY c
    (True, True) -> do
      putInt16be $ fromIntegral $ componentX c
      putInt16be $ fromIntegral $ componentY c
  when (flag .&. (shift 1 3 + shift 1 6 + shift 1 7) /= 0) $
    putShortFrac $ componentXX c
  when (byteAt flag 7) $ do
    putShortFrac $ componentXY c
    putShortFrac $ componentYX c
  when (flag .&. (shift 1 6 + shift 1 7) /= 0) $
    putShortFrac $ componentYY c
  for_ (componentInstructions c) $ \instr -> do
    putWord16be $ fromIntegral $ V.length instr
    traverse_ putWord8 instr
      where
        flag = makeFlag [
          if matchPoints c
          then componentX c > 0xff ||
               componentY c > 0xff
          else (not $ isShortInt $ componentX c) ||
               (not $ isShortInt $ componentY c),
          not $ matchPoints c,
          roundXYtoGrid c,
          componentXX c /= 1 &&
          componentXX c == componentYY c &&
          componentXY c == 0 && componentYX c == 0,
          False,
          more,
          componentXX c /= componentYY c &&
          componentXY c == 0 && componentYX c == 0,
          componentXY c /= 0 || componentYX c /= 0,
          isJust (componentInstructions c),
          useMyMetrics c,
          overlapCompound c,
          scaledComponentOffset c,
          not $ scaledComponentOffset c]

getComponent :: Get (GlyphComponent, Bool)
getComponent = do
  flag <- getWord16be
  gID <- getWord16be
  (cX, cY) <-
    if | byteAt flag 0 && byteAt flag 1 ->
           liftM2 (,)
           (fromIntegral <$> getInt16be)
           (fromIntegral <$> getInt16be)
       | byteAt flag 0 ->
           liftM2 (,)
           (fromIntegral <$> getWord16be)
           (fromIntegral <$> getWord16be)
       | byteAt flag 1 ->
           liftM2 (,)
           (fromIntegral <$> getInt8)
           (fromIntegral <$> getInt8)
       | otherwise ->
           liftM2 (,)
           (fromIntegral <$> getWord8)
           (fromIntegral <$> getWord8)
  (tXX, tXY, tYX, tYY) <-
    if | byteAt flag 3 -> do
           x <- ShortFrac <$> getInt16be
           return (x, 0, 0, x)
       | byteAt flag 6 -> do
           x <- ShortFrac <$> getInt16be
           y <- ShortFrac <$> getInt16be
           return (x, 0, 0, y)
       | byteAt flag 7 -> do
           xx <- ShortFrac <$> getInt16be
           xy <- ShortFrac <$> getInt16be
           yx <- ShortFrac <$> getInt16be
           yy <- ShortFrac <$> getInt16be
           return (xx, xy, yx, yy)
       | otherwise -> return (1, 0, 0, 1)
  instructions <- 
    if byteAt flag 8
    then Just <$> do
      l <- fromIntegral <$> getWord16be
      V.replicateM l getWord8
    else return Nothing
  return (
    GlyphComponent (fromIntegral gID) instructions tXX tXY tYX tYY
      cX cY (not $ byteAt flag 1) (byteAt flag 2)
      (byteAt flag 9) (byteAt flag 10) (byteAt flag 11),
    byteAt flag 5) -- more components

