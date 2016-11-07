{-# LANGUAGE MultiWayIf #-}
module Truetype.Fileformat.Glyph where
import Truetype.Fileformat.Types
import qualified Data.Vector as V
import Data.Foldable (traverse_, for_)
import Control.Monad
import Data.Function (fix)
import Data.Word
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
  advanceWidth :: Word,
  leftSideBearing :: Int,
  glyphXmin :: FWord,
  glyphYMin :: FWord,
  glyphXmax :: FWord,
  glyphYmax :: FWord,
  glyphOutlines :: GlyphOutlines}

data GlyphOutlines =
  GlyphContours [[CurvePoint]] Instructions |
  CompositeGlyph [GlyphComponent]


-- | @CurvePoint x y onCurve@: Points used to describe the outline
-- using lines and quadratic beziers.  Coordinates are relative to the
-- previous point.  If two off-curve points follow each other, an
-- on-curve point is added halfway between.
data CurvePoint = CurvePoint Int Int Bool

-- | TODO: make a proper datatype for instructions.
type Instructions = V.Vector Word8

data GlyphComponent =
  GlyphComponent {
  componentID :: Int,
  componentInstructions :: Maybe Instructions,
  -- | transformation matrix for scaling the glyph
  componentXX :: F2Dot14,
  -- | transformation matrix for scaling the glyph
  componentXY :: F2Dot14,
  -- | transformation matrix for scaling the glyph
  componentYX :: F2Dot14,
  -- | transformation matrix for scaling the glyph
  componentYY :: F2Dot14,
  -- | if matchPoints is `True`, index of matching point in compound
  -- being constructed, otherwise x shift.  This offset is unscaled
  -- (microsoft bit set) in the rasterizer.
  componentX :: Int,
  -- | if matchPoints is `True`, index of matching point in compound,
  -- otherwise y shift.  If scaledComponentOffset is False, this
  -- offset is unscaled (microsoft and opentype default) in the
  -- rasterizer.
  componentY :: Int,
    -- | see previous
  matchPoints :: Bool,
  -- | For the xy values if the preceding is false.
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
  -- set to False (opentype default), unless the font is meant to work
  -- with old apple software.
  scaledComponentOffset :: Bool
}

newtype GlyphIntern = GlyphIntern Glyph

instance Binary GlyphIntern where
  put (GlyphIntern (Glyph _ _ xmin ymin xmax ymax outlines)) = do
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

  get = do
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
    return $ GlyphIntern $
      Glyph 0 0 xmin ymin xmax ymax outlines

isShort :: Int -> Bool
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
         
contourFlags :: [CurvePoint] -> [Word8]
contourFlags [] = []
contourFlags cps@(CurvePoint x y oc: cps2) =
  if null cps
  then [firstFlag]
  else firstFlag : zipWith pairFlags cps cps2
       where
         firstFlag = fromIntegral $
           makeFlag [oc, isShort x, isShort y, False, 
                     isShort x && x > 0,
                     isShort y && y > 0]


pairFlags :: CurvePoint -> CurvePoint -> Word8  
pairFlags (CurvePoint x1 y1 _) (CurvePoint x2 y2 oc) =
  fromIntegral $ 
  makeFlag [oc, sx && not eqX, sy && not eqY, False,
            if sx then x2 > 0 else eqX,
            if sy then y2 > 0 else eqY]
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
  traverse_ (putWord8.fromIntegral) endPts
  putWord8 $ fromIntegral $ V.length instr
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

getXcoords, getYcoords :: [Word8] -> Int -> Get [Int]
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
  | byteAt f 5 = (prev:) <$> getXcoords r prev
  | otherwise = do
      y <- fromIntegral <$> getWord16be
      (y:) <$> getYcoords r y

getPoint :: Int -> Int -> Word8 -> CurvePoint
getPoint x y flag = CurvePoint x y (byteAt flag 0)

reGroup :: [a] -> [Int] -> [[a]]
reGroup _ []  = []
reGroup l (n:ns) = c : reGroup r ns
  where
    (c, r) = splitAt n l
  
getContour :: Int -> Get GlyphOutlines
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
    putWord16be $ componentXX c
  when (byteAt flag 7) $ do
    putWord16be $ componentXY c
    putWord16be $ componentYX c
  when (flag .&. (shift 1 6 + shift 1 7) /= 0) $
    putWord16be $ componentYY c
  for_ (componentInstructions c) $ \instr -> do
    putWord16be $ fromIntegral $ V.length instr
    traverse_ putWord8 instr
      where
        flag = makeFlag [
          if matchPoints c
          then componentX c <= 0xff &&
               componentY c <= 0xff
          else (not $ isShortInt $ componentX c) &&
               (not $ isShortInt $ componentY c),
          not $ matchPoints c,
          roundXYtoGrid c,
          componentXX c /= 0x4000 &&
          componentXX c == componentYY c &&
          componentXY c == 0 && componentYX c == 0,
          False,
          more,
          componentXX c /= componentYY c &&
          componentXY c == 0 && componentYX c == 0,
          componentXX c /= componentYY c &&
          (componentXY c /= 0 || componentYX c /= 0),
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
           x <- getWord16be
           return (x, 0, 0, x)
       | byteAt flag 6 -> do
           x <- getWord16be
           y <- getWord16be
           return (x, 0, 0, y)
       | byteAt flag 7 -> do
           xx <- getWord16be
           xy <- getWord16be
           yx <- getWord16be
           yy <- getWord16be
           return (xx, xy, yx, yy)
       | otherwise -> return (1, 0, 0, 1)
  instructions <- 
    if byteAt flag 8
    then Just <$> do
      l <- fromIntegral <$> getWord8
      V.replicateM l getWord8
    else return Nothing
  return (
    GlyphComponent (fromIntegral gID) instructions tXX tXY tYX tYY
      cX cY (not $ byteAt flag 1) (byteAt flag 2)
      (byteAt flag 9) (byteAt flag 10) (byteAt flag 11),
    byteAt flag 5) -- more components
