module Truetype.Fileformat.OS2
where
import Truetype.Fileformat.Types
import Data.Word
import Data.Int
import Data.Char
import Data.Binary.Put
import Data.Binary.Get
import Data.Foldable

-- | The OS/2 table consists of a set of metrics that are required in
-- OpenType fonts.  For a description of these fields see:
-- https://www.microsoft.com/typography/otspec/os2.htm#vendid
data OS2Table = OS2Table {
  os2version :: Word16,
  xAvgCharWidth :: Int16,
  usWeightClass :: Word16,
  usWidthClass :: Word16,
  fsType :: Word16,
  ySubscriptXSize :: Int16,
  ySubscriptYSize :: Int16,
  ySubscriptXOffset :: Int16,
  ySubscriptYOffset :: Int16,
  ySuperscriptXSize :: Int16,
  ySuperscriptYSize :: Int16,
  ySuperscriptXOffset :: Int16,
  ySuperscriptYOffset :: Int16,
  yStrikeoutSize :: Int16,
  yStrikeoutPosition :: Int16,
  bFamilyClass :: Int16,
  bFamilyType :: Int8,
  bSerifStyle :: Int8,
  bWeight :: Int8,
  bProportion :: Int8,
  bContrast :: Int8,
  bStrokeVariation :: Int8,
  bArmStyle :: Int8,
  bLetterform :: Int8,
  bMidline :: Int8,
  bXHeight :: Int8,
  ulUnicodeRange1 :: Word32,
  ulUnicodeRange2 :: Word32,
  ulUnicodeRange3 :: Word32,
  ulUnicodeRange4 :: Word32,
  achVendID :: Word32,
  fsSelection :: Word16,
  usFirstCharIndex :: Word16,
  usLastCharIndex :: Word16,
  sTypoAscender :: Int16,
  sTypoDescender :: Int16,
  sTypoLineGap :: Int16,
  usWinAscent :: Word16,
  usWinDescent :: Word16,
  ulCodePageRange1 :: Word32,
  ulCodePageRange2 :: Word32,
  sxHeight :: Int16,
  sCapHeight :: Int16,
  usDefaultChar :: Word16,
  usBreakChar :: Word16,
  usMaxContext :: Word16,
  usLowerOpticalPointSize :: Word16,
  usUpperOpticalPointSize :: Word16
  }

getOS2Table :: Get OS2Table
getOS2Table = do
  version <- getWord16be
  partial1 <- OS2Table version <$> getInt16be
    <*> getWord16be <*> getWord16be <*> getWord16be
    <*> getInt16be <*> getInt16be <*> getInt16be
    <*> getInt16be <*> getInt16be <*> getInt16be
    <*> getInt16be <*> getInt16be <*> getInt16be
    <*> getInt16be <*> getInt16be <*> getInt8
    <*> getInt8 <*> getInt8 <*> getInt8 <*> getInt8
    <*> getInt8 <*> getInt8 <*> getInt8 <*> getInt8
    <*> getInt8 <*> getWord32be <*> getWord32be <*> getWord32be
    <*> getWord32be <*> getWord32be <*> getWord16be
    <*> getWord16be <*> getWord16be <*> getInt16be
    <*> getInt16be <*> getInt16be <*> getWord16be <*> getWord16be
  if version == 0 then
    return $ partial1 0 0 0 0 0 0 0 0 0
    else do
    partial2 <- partial1 <$> getWord32be <*> getWord32be
    if version == 1 then
      return $ partial2 0 0 0 0 0 0 0
      else do
      partial3 <- partial2 <$> getInt16be <*> getInt16be <*>
        getWord16be <*> getWord16be <*> getWord16be
      if version < 5 then
        return $ partial3 0 0
        else partial3 <$> getWord16be <*> getWord16be

putOS2Table :: OS2Table -> PutM Int
putOS2Table (OS2Table version f2 f3 f4 f5 f6 f7 f8 f9 f10
             f11 f12 f13 f14 f15 f16 f17 f18 f19 f20
             f21 f22 f23 f24 f25 f26 f27 f28 f29 f30
             f31 f32 f33 f34 f35 f36 f37 f38 f39 f40
             f41 f42 f43 f44 f45 f46 f47 f48) =
  do putWord16be version
     putInt16be f2
     putWord16be f3
     putWord16be f4
     putWord16be f5
     putInt16be f6
     putInt16be f7
     putInt16be f8
     putInt16be f9
     putInt16be f10
     putInt16be f11
     putInt16be f12
     putInt16be f13
     putInt16be f14
     putInt16be f15
     putInt16be f16
     putInt8 f17
     putInt8 f18
     putInt8 f19
     putInt8 f20
     putInt8 f21
     putInt8 f22
     putInt8 f23
     putInt8 f24
     putInt8 f25
     putInt8 f26
     putWord32be f27
     putWord32be f28
     putWord32be f29
     putWord32be f30
     putWord32be f31
     putWord16be f32
     putWord16be f33
     putWord16be f34
     putInt16be f35
     putInt16be f36
     putInt16be f37
     putWord16be f38
     putWord16be f39
     if version == 0
       then return 78
       else do
       putWord32be f40
       putWord32be f41
       if version == 1
         then return 86
         else do
         putInt16be f42
         putInt16be f43
         putWord16be f44
         putWord16be f45
         putWord16be f46
         if version <= 4
           then return 96
           else do
           putWord16be f47
           putWord16be f48
           return 100

