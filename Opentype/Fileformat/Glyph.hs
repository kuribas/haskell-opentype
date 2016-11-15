{-# LANGUAGE MultiWayIf, TupleSections #-}
module Opentype.Fileformat.Glyph where
import Opentype.Fileformat.Types
import Opentype.Fileformat.Maxp
import Opentype.Fileformat.Hhea
import Opentype.Fileformat.Head
import qualified Data.Vector as V
import Data.Foldable (traverse_, for_)
import Control.Monad
import Data.Function (fix)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
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
  deriving Show

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
-- using lines and quadratic beziers.  Coordinates are absolute (not
-- relative).  If two off-curve points follow each other, an on-curve
-- point is added halfway between.
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

emptyGlyph :: Glyph
emptyGlyph = Glyph 0 0 0 0 0 0 (GlyphContours [] V.empty)

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

readGlyphLocs :: Bool -> Int -> Get [Int]
readGlyphLocs long n =
  replicateM (n+1) $ 
  if long then fromIntegral <$> getWord32be
  else (*2).fromIntegral <$> getWord16be
 
readGlyphTable :: [(Int, Int)] -> [(Word16, Int16)] -> Strict.ByteString
               -> Either String (V.Vector Glyph)
readGlyphTable glyphSizes hmetrics glyfBs =
  V.fromList <$> zipWithM readGlyph glyphSizes hmetrics
  where
    readGlyph (offset, size) (aw, lsb)
      | offset + size > Strict.length glyfBs =
        Left "glyph past table bounds."
      | otherwise =
        case runGetOrFail getGlyph (Lazy.fromStrict $ Strict.drop offset glyfBs) of
          Left (_, _, err) -> Left err
          Right (_, _, g) -> Right $ g {advanceWidth = aw, leftSideBearing = lsb}

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
writeLoca :: V.Vector Int -> PutM Bool
writeLoca vec
  | V.last offsets > 0xffff =
     do traverse_ (putWord32be . fromIntegral) offsets
        return True
  | otherwise =
     do traverse_ (putWord16be . (`quot` 2) . fromIntegral) offsets
        return False
  where
    offsets = V.scanl (+) 0 vec
    
writeHmtx :: V.Vector Glyph -> PutM Int
writeHmtx gs
  | V.null gs = return 0
  | otherwise = 
    do traverse_ (\g -> do putWord16be (advanceWidth g)
                           putInt16be (leftSideBearing g)
                 ) dbl
       traverse_ (putInt16be.leftSideBearing) sngl
       return (len-tl)
     where
      findTail i cnt
        | i < 0 = cnt
        | advanceWidth (V.unsafeIndex gs i) == aw =
            findTail (i-1) (cnt+1)
        | otherwise = cnt
      aw = advanceWidth (V.unsafeLast gs)
      len = V.length gs
      tl = findTail (len-2) 0
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
               putWord8 $ fromIntegral $ length as
     putCompressFlags r2
       where
         (as, r2) = span (== a) r
         
contourFlag :: CurvePoint -> Word8  
contourFlag (CurvePoint x y oc) =
  fromIntegral $ 
  makeFlag [oc, sx && x /= 0, sy && y /= 0, False,
            x == 0 || (sx && x >= 0),
            y == 0 || (sy && y >= 0)]
  where
    sx = isShort x
    sy = isShort y

firstFlag :: CurvePoint -> Word8
firstFlag (CurvePoint x y oc) =
  fromIntegral $
  makeFlag [oc, sx, sy, False, sx && x >= 0, sy && y >= 0]
  where sx = isShort x
        sy = isShort y
        
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
    allPts = case concat points of
      [] -> []
      pts@(p: pts2) -> p: zipWith subCoord pts2 pts
    subCoord (CurvePoint x1 y1 _) (CurvePoint x2 y2 on) =
      CurvePoint (x2-x1) (y2-y1) on
    flags = case allPts of
      [] -> []
      (p:pts) -> firstFlag p : map contourFlag pts

getFlags :: Int -> Get [Word8]
getFlags n
  | n <= 0 = return []
  | otherwise = do
      flag <- getWord8
      if flag .&. 8 /= 0 && n > 1
        then do m <- fromIntegral <$> getWord8
                (replicate (min (m+1) n) flag ++) <$> getFlags (n-m-1)
        else (flag:) <$> getFlags (n-1)

getXcoords, getYcoords :: [Word8] -> Get [FWord]
getXcoords [] = return []
getXcoords (f:r) 
  | byteAt f 1 = do
      x <- getWord8
      let x' | byteAt f 4 = fromIntegral x
             | otherwise = - fromIntegral x
      (x':) <$> getXcoords r
  | byteAt f 4 = (0:) <$> getXcoords r
  | otherwise = do
      x <- fromIntegral <$> getInt16be
      (x:) <$> getXcoords r

getYcoords [] = return []
getYcoords (f:r) 
  | byteAt f 2 = do
      y <- getWord8
      let y' | byteAt f 5 = fromIntegral y
             | otherwise = - fromIntegral y
      (y':) <$> getYcoords r
  | byteAt f 5 = (0:) <$> getYcoords r
  | otherwise = do
      y <- fromIntegral <$> getInt16be
      (y:) <$> getYcoords r

getPoint :: FWord -> FWord -> Word8 -> CurvePoint
getPoint x y flag = CurvePoint x y (byteAt flag 0)

reGroup :: [a] -> [Int] -> [[a]]
reGroup _ []  = []
reGroup l (n:ns) = c : reGroup r ns
  where
    (c, r) = splitAt n l

toOffsets :: (Num a) => [a] -> [a]
toOffsets [] = []
toOffsets (x:xs) = scanl (-) x xs
  
getContour :: Int -> Get GlyphOutlines
getContour 0 =  return $ GlyphContours [] V.empty
getContour nContours = do
  lastPts <- replicateM nContours (fromIntegral <$> getWord16be)
  iLen <- fromIntegral <$> getWord16be
  instructions <- V.replicateM iLen getWord8
  flags <- getFlags $ last lastPts + 1
  xCoords <- toOffsets <$> getXcoords flags
  yCoords <- toOffsets <$> getYcoords flags
  let coords = zipWith3 getPoint xCoords yCoords flags
      contours = reGroup coords $
                 zipWith (-) lastPts ((-1):lastPts)
  return $ GlyphContours contours instructions

isShortInt :: Int -> Bool
isShortInt x = x <= 127 && x >= -128

glyphExtent, glyphRsb :: Glyph -> Int16
glyphRsb g =
  fromIntegral (advanceWidth g) - glyphExtent g
  
glyphExtent glyf = leftSideBearing glyf + (glyphXmax glyf - glyphXmin glyf)

updateHhea :: V.Vector Glyph -> HheaTable -> HheaTable
updateHhea v h = V.foldl updateHhea1
                 (h {advanceWidthMax     = minBound,
                     minLeftSideBearing  = maxBound,
                     minRightSideBearing = maxBound,
                     xMaxExtent          = minBound})
                 v
  

updateMinMax :: (FWord, FWord, FWord, FWord)
             -> Glyph -> (FWord, FWord, FWord, FWord)
updateMinMax (xmin, ymin, xmax, ymax) g =
  (min xmin (glyphXmin g),
   min ymin (glyphYmin g),
   max xmax (glyphXmax g),
   max ymax (glyphYmax g))

updateHead :: V.Vector Glyph -> HeadTable -> HeadTable
updateHead vec headTbl =
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
updateMaxp :: V.Vector Glyph -> MaxpTable -> MaxpTable
updateMaxp vec tbl = V.foldl (updateMaxp1 vec)
                     (tbl {numGlyphs = 0,
                           maxPoints = 0,
                           maxContours = 0,
                           maxComponentPoints = 0,
                           maxComponentContours = 0,
                           maxComponentElements = 0,
                           maxComponentDepth = 0})
                     vec

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
               -> Int -> Bool -> V.Vector Glyph -> Glyph -> Word16
overComponents f h maxD d v g
  | maxD <= 0 = 0
  | otherwise =
    case glyphOutlines g of
      GlyphContours p _
        | d -> 0
        | otherwise -> f p
      CompositeGlyph comps ->
        h $ map overSub comps
        where overSub comp = case v V.!? componentID comp of
                Nothing -> 0
                Just g2 -> overComponents f h (maxD-1) False v g2 
  
glyfPoints, glyfContours, componentRefs, componentDepth, componentPoints, componentContours :: V.Vector Glyph -> Glyph -> Word16

glyfPoints  =
  overComponents (sum . map (fromIntegral.length)) (const 0) 2 False

glyfContours =
  overComponents (fromIntegral.length) (const 0) 2 False

componentRefs =
  overComponents (const 0) (fromIntegral.length) 2 True

componentPoints =
  overComponents (sum . map (fromIntegral . length)) sum 10 True

componentDepth =
  overComponents (const 0) ((+1).maximum) 10 True

componentContours =
  overComponents (fromIntegral.length) sum 10 True

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

