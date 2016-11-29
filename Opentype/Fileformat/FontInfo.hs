-- | The goal of this module is to make it easier to fill in the
-- global font information, without having to write to multiple
-- tables, and ensuring consistency.  For most fields sensible defaults
-- are provided.
module Opentype.Fileformat.FontInfo (FontInfo(..), Weight(..),
                                     Width(..), Slant(..), Decoration(..),
                                     EmbedLicence(..), infoToTables)
where
import Opentype.Fileformat
import Opentype.Fileformat.Types
import Data.Maybe
import Data.Word
import Data.Time
import Data.Char
import Data.Bits
import Data.Binary.Put
import Data.Foldable
import Text.Printf
import Lens.Micro
import Lens.Micro.Extras
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict

data Weight = Thin | ExtraLight | Light | NormalWeight | MediumWeight
            | SemiBold | Bold | ExtraBold | Heavy
            deriving (Eq, Ord)

instance Show Weight where
  show Thin = "Thin"
  show ExtraLight = "Extra Light"
  show Light = "Light"
  show NormalWeight = "Regular"
  show MediumWeight = "Medium"
  show SemiBold = "Semibold"
  show Bold = "Bold"
  show ExtraBold = "Extra Bold"
  show Heavy = "Heavy"

data Width = UltraCondensed | ExtraCondensed | Condensed | SemiCondensed
           | MediumWidth | SemiExpanded | Expanded | ExtraExpanded | UltraExpanded
           deriving (Eq, Ord)

instance Show Width where
  show UltraCondensed = "Ultra Condensed"
  show ExtraCondensed = "Extra Condensed"
  show Condensed = "Condensed"
  show SemiCondensed = "Semi Condensed"
  show MediumWidth = "Regular"
  show SemiExpanded = "Semi Expanded"
  show Expanded = "Expanded"
  show ExtraExpanded = "Extra Expanded"
  show UltraExpanded = "Ultra Expanded"
  
data Slant = Italic | Oblique | NoSlant
  deriving Eq

instance Show Slant where
  show Italic = "Italic"
  show Oblique = "Oblique"
  show NoSlant = "Regular"

data Decoration = Underscore | Negative | Outlined | StrikeOut | Shadow
  deriving Eq

data EmbedLicence =
  -- | Fonts must not be modified, embedded or exchanged in any manner
  -- without first obtaining permission of the legal owner.  Caution:
  -- For Restricted License embedding to take effect, it must be the
  -- only level of embedding selected.
  RestrictedEmbedding |
  -- | The font may be embedded, and temporarily loaded on the remote
  -- system. Documents containing Preview & Print fonts must be opened
  -- “read-only;” no edits can be applied to the document.
  PrintPreview |
  -- | The font may be embedded but must only be installed temporarily
  -- on other systems. In contrast to Preview & Print fonts, documents
  -- containing Editable fonts may be opened for reading, editing is
  -- permitted, and changes may be saved.
  EditEmbed |
  -- | When this bit is set, the font may not be subsetted prior to
  -- embedding. Other embedding restrictions also apply.
  NoSubsetEmbed |
  -- | When this bit is set, only bitmaps contained in the font may be
  -- embedded. No outline data may be embedded. If there are no
  -- bitmaps available in the font, then the font is considered
  -- unembeddable and the embedding services will fail. Other
  -- embedding restrictions specified in bits 0-3 and 8 also apply.
  OnlyBitmapEmbed

licenceBit :: EmbedLicence -> Int
licenceBit RestrictedEmbedding = 0x0002
licenceBit PrintPreview = 0x0004
licenceBit EditEmbed = 0x0008
licenceBit NoSubsetEmbed = 0x0100
licenceBit OnlyBitmapEmbed = 0x0200

embeddedBits :: [EmbedLicence] -> Word16
embeddedBits = fromIntegral . sum . map licenceBit

-- |  Currently only english strings are supported.
data FontInfo = FontInfo {
  -- | Font Family name, without subfamily classifiers such as /Bold/, /Italic/, etc...
  fontFamily :: String,
  -- | Font version, multiplied by 1000, for example use 1500 for
  -- version 1.5
  fontVersion :: Int,
  -- | Number of units in an em-square.  Should be a power of 2.
  -- Typical values are 1024 or 2048.
  fontUnitsPerEm :: FWord,
  -- | Distance between the bottom of the em-square and the baseline
  -- in font design units.
  fontEmBase :: FWord,
  -- | Linegap in font design units.  Defined as distance between
  -- baselines - height of em square (`fontUnitsPerEm`).
  fontLineGap :: FWord,
  -- | Weight of the font.  /default/: `NormalWeight`
  fontWeight :: Weight,
  -- | Width of the font.  /default/: `MediumWidth`
  fontWidth :: Width,
  -- | Slant of the font.  /default/: `NoSlant`
  fontSlant :: Slant,
  -- | fixed width font.  /default/: False
  fontMonospaced :: Maybe Bool,
  -- | Smallest readable size in pixels.  /default/: 6
  fontLowestRecPPEM :: Maybe Int,
  -- | talic angle in counter-clockwise degrees from the
  -- vertical. Zero for upright text, negative for text that leans to
  -- the right (forward).  /default/: 0
  fontItalicAngle :: Maybe Double,
  -- | The amount by which a slanted highlight on a glyph needs to be
  -- shifted to produce the best appearance. /default/: 0
  fontCaretOffset :: Maybe FWord,
  -- | The recommended size in font design units for subscripts for
  -- this font. /default/: (fontUnitsPerEM/2, fontUnitsPerEM/2)
  fontSubScriptSize :: Maybe (FWord, FWord),
  -- | The recommended offset in font design units for subscripts for
  -- this font. /default/: (0, -fontUnitsPerEM/4)
  fontSubScriptOffset :: Maybe (FWord, FWord),
  -- | The recommended size in font design units for superscripts for
  -- this font. /default/: (fontUnitsPerEM/2, fontUnitsPerEM/2)
  fontSuperScriptSize :: Maybe (FWord, FWord),
  -- | The recommended offset in font design units for superscripts for
  -- this font. /default/: (0, (fontUnitsPerEM-fontEmBase)/2)
  fontSuperScriptOffset :: Maybe (FWord, FWord),
  -- | ndicates font embedding licensing rights for the
  -- font. Embeddable fonts may be stored in a document. When a
  -- document with embedded fonts is opened on a system that does not
  -- have the font installed (the remote system), the embedded font
  -- may be loaded for temporary (and in some cases, permanent) use on
  -- that system by an embedding-aware application. Embedding
  -- licensing rights are granted by the vendor of the font.
  -- /default/: []
  fontEmbeddingLicence :: Maybe [EmbedLicence],
  -- | Width of the strikeout stroke in font design units.  This field
  -- should normally be the width of the em dash for the current
  -- font. If the size is one, the strikeout line will be the line
  -- represented by the strikeout position field. If the value is two,
  -- the strikeout line will be the line represented by the strikeout
  -- position and the line immediately above the strikeout
  -- position. /default/: fontUnitsPerEm/20 
  fontStrikoutSize :: Maybe FWord,
  -- | The position of the top of the strikeout stroke relative to the
  -- baseline in font design units.  Positive values represent
  -- distances above the baseline, while negative values represent
  -- distances below the baseline. A value of zero falls directly on
  -- the baseline, while a value of one falls one pel above the
  -- baseline. The value of strikeout position should not interfere
  -- with the recognition of standard characters, and therefore should
  -- not line up with crossbars in the
  -- font. /default/. fontUnitsPerEm/5.5
  fontStrikeoutPosition :: Maybe FWord,
  -- | The font class and font subclass are registered values assigned
  -- by IBM to each font family. This parameter is intended for use in
  -- selecting an alternate font when the requested font is not
  -- available. The font class is the most general and the font
  -- subclass is the most specific. The high byte of this field
  -- contains the family class, while the low byte contains the family
  -- subclass. <https://www.microsoft.com/typography/otspec/ibmfc.htm
  -- More_information_about_this_field>.
  -- /default/: (0,0)
  fontFamilyClass :: Maybe (Int, Int),
  -- | The four character identifier for the vendor of the given type
  -- face. This is not the royalty owner of the original artwork. This
  -- is the company responsible for the marketing and distribution of
  -- the typeface that is being classified. It is reasonable to assume
  -- that there will be 6 vendors of ITC Zapf Dingbats for use on
  -- desktop platforms in the near future (if not already). It is also
  -- likely that the vendors will have other inherent benefits in
  -- their fonts (more kern pairs, unregularized data, hand hinted,
  -- etc.). This identifier will allow for the correct vendor's type
  -- to be used over another, possibly inferior, font file. The Vendor
  -- ID value is not required.
  -- /default/: (' ', ' ', ' ', ' ')
  fontVendorID :: Maybe (Char, Char, Char, Char),
  -- | Panose-1 Classification.  /default/: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  fontPanose :: Maybe (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
  -- | Supported Unicode Ranges: See
  -- https://www.microsoft.com/typography/otspec/os2.htm#ur.
  -- /default/ (3, 0, 0, 0)
  fontUnicodeRanges :: Maybe (Word32, Word32, Word32, Word32),
  -- | Supported Codepage Ranges. See
  -- https://www.microsoft.com/typography/otspec/os2.htm#cpr.
  -- /default/: (1, 0)
  fontCodepageRanges :: Maybe (Word32, Word32),
  -- | This metric specifies the distance between the baseline and the
  -- approximate height of non-ascending lowercase letters measured in
  -- FUnits.  /default/ height of glyph at U+0078 (LATIN SMALL LETTER
  -- X)
  fontXHeight :: Maybe FWord,
  -- | This metric specifies the distance between the baseline and the
  -- approximate height of uppercase letters measured in FUnits.
  -- /default/ height of glyph at U+0048 (LATIN CAPITAL LETTER H).
  fontCapHeight :: Maybe FWord,
  -- | This field is used for fonts with multiple optical styles.
  -- 
  -- This value is the lower value of the size range for which this
  -- font has been designed. The units for this field are TWIPs
  -- (one-twentieth of a point, or 1440 per inch). The value is
  -- inclusive—meaning that that font was designed to work best at
  -- this point size through, but not including, the point size
  -- indicated by usUpperOpticalPointSize. When used with other
  -- optical fonts that set usLowerOpticalPointSize and
  -- usUpperOpticalPointSize, it would be expected that another font
  -- has this same value as this entry in the usUpperOpticalPointSize
  -- field, unless this font is designed for the lowest size
  -- range. The smallest font in an optical size set should set this
  -- value to 0.When working across multiple optical fonts, there
  -- should be no intentional gaps or overlaps in the
  -- ranges. usLowerOpticalPointSize must be less than
  -- usUpperOpticalPointSize. The maximum valid value is 0xFFFE.
  -- /default/: 0
  fontLowerOpticalPointSize :: Maybe Int,
  -- | fontUpperOptThis field is used for fonts with multiple optical styles.
  --
  -- This value is the upper value of the size range for which this
  -- font has been designed. The units for this field are TWIPs
  -- (one-twentieth of a point, or 1440 per inch). The value is
  -- exclusive—meaning that that font was designed to work best below
  -- this point size down to the usLowerOpticalPointSize
  -- threshold. When used with other optical fonts that set
  -- usLowerOpticalPointSize and usUpperOpticalPointSize, it would be
  -- expected that another font has this same value as this entry in
  -- the usLowerOpticalPointSize field, unless this font is designed
  -- for the highest size range. The largest font in an optical size
  -- set should set this value to 0xFFFF, which is interpreted as
  -- infinity. When working across multiple optical fonts, there
  -- should be no intentional or overlaps left in the
  -- ranges. usUpperOpticalPointSize must be greater than
  -- usLowerOpticalPointSize. The minimum valid value for this field
  -- is 2 (two). The largest possible inclusive point size represented
  -- by this field is 3276.65 points, any higher values would be
  -- represented as infinity.  /default/: 0xffff
  fontUpperOpticalPointSize :: Maybe Int,
  -- | This is the suggested distance of the top of the underline from
  -- the baseline (negative values indicate below baseline).  The
  -- PostScript definition of this FontInfo dictionary key (the y
  -- coordinate of the center of the stroke) is not used for
  -- historical reasons. The value of the PostScript key may be
  -- calculated by subtracting half the underlineThickness from the
  -- value of this field. /default/: /default/ -fontUnitsPerEm/8
  fontUnderlinePosition :: Maybe FWord,
  -- | suggested values for the underline thickness. /default/ fontUnitsPerEm/10
  fontUnderlineThickness :: Maybe FWord,
  -- | This field is used when the font has a subfamily other than
  -- Weight, Width or Slant.  Should not include any width, weight or
  -- slant descriptions. /default/: ""
  fontSubFamilyExtra :: String,
  -- | Set if the font has any of these decorations.  /default/: []
  fontDecoration :: [Decoration],
  -- | Copyright notice.  Default: []
  fontCopyright :: String,
  -- | Unique font identifier
  fontID :: String,
  -- | postscriptName.  /default/: full name with '-' substituted for spaces.
  fontPsName :: String,
  -- | trademark. /default/: ""
  fontTrademark :: String,
  -- | Manufacturer Name.  /default/: ""
  fontManufacturer :: String,
  -- | name of the designer of the typeface.  /default/: ""
  fontDesigner :: String,
  -- | description of how the font may be legally used, or different
  -- example scenarios for licensed use. This field should be written
  -- in plain language, not legalese.  /default/: ""
  fontLicence :: String,
  -- | description of the typeface. Can contain revision information,
  -- usage recommendations, history, features, etc.
  fontDescription :: String,
  -- | URL where additional licensing information can be found.
  -- /default/ ""
  fontLicenceUrl :: String,
  -- | URL of typeface designer (with protocol, e.g., http://, ftp://).
  fontDesignerUrl :: String,
  -- | URL of font vendor (with protocol, e.g., http://, ftp://). If a
  -- unique serial number is embedded in the URL, it can be used to
  -- register the font.
  fontVendorUrl :: String,
  -- | This can be the font name, or any other text that the designer
  -- thinks is the best sample to display the font in.  /default/: ""
  fontSampleText :: String,
  -- | This ID, if used in the CPAL table’s Palette Labels Array,
  -- specifies that the corresponding color palette in the CPAL table
  -- is appropriate to use with the font when displaying it on a light
  -- background such as white. Name table strings for this ID specify
  -- the user interface strings associated with this
  -- palette. /default/ ""
  fontLightPalette :: String,
  -- | Dark Backgound Palette. This ID, if used in the CPAL table’s
  -- Palette Labels Array, specifies that the corresponding color
  -- palette in the CPAL table is appropriate to use with the font
  -- when displaying it on a dark background such as black. Name table
  -- strings for this ID specify the user interface strings associated
  -- with this palette
  fontDarkPalette :: String,
  -- | font creation time
  fontCreated :: UTCTime,
  -- | font modification time.  /default/: creation time.
  fontModified :: Maybe UTCTime
  }

weightClass :: Weight -> Word16
weightClass Thin = 100
weightClass ExtraLight = 200
weightClass Light = 300
weightClass NormalWeight = 400
weightClass MediumWeight = 500
weightClass SemiBold = 600
weightClass Bold = 700
weightClass ExtraBold = 800
weightClass Heavy = 900

widthClass :: Width -> Word16
widthClass UltraCondensed = 1
widthClass ExtraCondensed = 2
widthClass Condensed = 3
widthClass SemiCondensed = 4
widthClass MediumWidth = 5
widthClass SemiExpanded = 6
widthClass Expanded = 7
widthClass ExtraExpanded = 8
widthClass UltraExpanded = 9

(+++) :: String -> String -> String
s +++ "" = s
s +++ t = s ++ " " ++ t

notRegular :: (Show a, Eq a) => a -> a -> String
notRegular reg sub =
  if reg == sub then "" else show sub

(///) :: Maybe c -> c -> c
(///) = flip fromMaybe

-- | Fill in default fontinfo values
infoToTables :: FontInfo -> (HeadTable, HheaTable, NameTable, PostTable, OS2Table)
infoToTables fi = (headTbl, hheaTbl, nameTbl, postTbl, os2Tbl)
  where
    headTbl = HeadTable {
      headVersion = 0x00010000,
      fontRevision = fromIntegral $ 
                     fromIntegral (fontVersion fi) * 0x00010000 `quot`
                     (1000 :: Integer),
      baselineYZero = True,
      sidebearingXZero = True,
      pointsizeDepend = False,
      integerScaling = False,
      alterAdvanceWidth = False,
      verticalFont = False,
      linguisticRenderingLayout = False,
      metamorphosisEffects = False,
      rightToLeftGlyphs = False,
      indicRearrangements = False,
      losslessFontData = False,
      convertedFont = False,
      clearTypeOptimized = False,
      lastResortFont = False,
      unitsPerEm = fromIntegral $ fontUnitsPerEm fi,
      created = fontCreated fi,
      modified = fontModified fi /// fontCreated fi,
      xMin = 0,
      yMin = 0,
      xMax = 0,
      yMax = 0,
      boldStyle = fontWeight fi > NormalWeight,
      italicStyle = fontSlant fi == Italic,
      underlineStyle = Underscore `elem` fontDecoration fi,
      outlineStyle = Outlined `elem` fontDecoration fi,
      shadowStyle = Shadow `elem` fontDecoration fi,
      condensedStyle = fontWidth fi < MediumWidth,
      extendedStyle = fontWidth fi > MediumWidth,
      lowerRecPPEM = (fromIntegral <$> fontLowestRecPPEM fi) /// 6,
      fontDirectionHint = 2,
      longLocIndices = False,
      glyphDataFormat = 0}
    hheaTbl = HheaTable {
      version = 0x00010000,
      ascent = 0,
      descent = 0,
      lineGap = 0,
      advanceWidthMax = 0,
      minLeftSideBearing = 0,
      minRightSideBearing = 0,
      xMaxExtent = 0,
      caretSlopeRise = case fontItalicAngle fi /// 0 of
          0 -> 1
          -90 -> 0
          a -> round $ cos (pi*a/180 + pi/2) * 2048,
      caretSlopeRun = case fontItalicAngle fi /// 0 of
          0 -> 0
          -90 -> 1
          a -> round $ sin (pi*a/180 + pi/2) * 2048,
      caretOffset = fontCaretOffset fi /// 0,
      numOfLongHorMetrics = 0}
    mkNameRecords _ "" = []
    mkNameRecords nid ns = 
      [NameRecord MacintoshPlatform 0 0 nid $
       Strict.pack $ map (fromIntegral . (.&.0xff) . ord) ns,
       NameRecord MicrosoftPlatform 1 0x0409 nid $
       Lazy.toStrict $ runPut (traverse_ (putWord16be . fromIntegral . ord) ns)]
    (versionMajor, versionMinor) = fontVersion fi `quotRem` 1000
    fullName = fontFamily fi +++ subFamily
    subFamily = fontSubFamilyExtra fi +++
                notRegular NormalWeight (fontWeight fi) +++
                notRegular MediumWidth (fontWidth fi) +++
                notRegular NoSlant (fontSlant fi)
    wsSubFamily
      | fontWeight fi == NormalWeight && fontSlant fi == NoSlant
      = "Regular"
      | otherwise = weightName +++ notRegular NoSlant (fontSlant fi)
      where
        weightName
          | fontWeight fi < NormalWeight = "Thin"
          | fontWeight fi > NormalWeight = "Bold"
          | otherwise = ""

    nameTbl = NameTable $
        concat [
          mkNameRecords 0 $ fontCopyright fi,
          mkNameRecords 1 $ fontFamily fi +++ fontSubFamilyExtra fi +++
            notRegular MediumWidth (fontWidth fi),
          mkNameRecords 2 wsSubFamily,
          mkNameRecords 3 $ fontID fi,
          mkNameRecords 4 fullName,
          mkNameRecords 5 $ printf "Version %d.%03d" versionMajor versionMinor,
          mkNameRecords 6 $ if null (fontPsName fi)
            then take 63 $ map (\c -> if c == ' ' then '-' else c) fullName
            else fontPsName fi,
          mkNameRecords 7 $ fontTrademark fi,
          mkNameRecords 8 $ fontManufacturer fi,
          mkNameRecords 9 $ fontDesigner fi,
          mkNameRecords 10 $ fontDescription fi,
          mkNameRecords 11 $ fontVendorUrl fi,
          mkNameRecords 12 $ fontDesignerUrl fi,
          mkNameRecords 13 $ fontLicence fi,
          mkNameRecords 14 $ fontLicenceUrl fi,
          mkNameRecords 16 $ fontFamily fi,
          mkNameRecords 17 subFamily,
          mkNameRecords 19 $ fontSampleText fi,
          mkNameRecords 21 $
            if null (fontSubFamilyExtra fi) then ""
            else fontFamily fi +++ fontSubFamilyExtra fi,
          mkNameRecords 22 $
            if null (fontSubFamilyExtra fi) then ""
            else notRegular NormalWeight (fontWeight fi) +++
                 notRegular MediumWidth (fontWidth fi) +++
                 notRegular NoSlant (fontSlant fi),
          mkNameRecords 23 $ fontLightPalette fi,
          mkNameRecords 24 $ fontDarkPalette fi]
    postTbl = PostTable {
      postVersion = PostTable2,
      italicAngle = round $ (fontItalicAngle fi /// 0)
                    * 0x00010000,
      underlinePosition = fontUnderlinePosition fi ///
                          (- fromIntegral (fontUnitsPerEm fi `quot` 8)),
      underlineThickness = fontUnderlineThickness fi ///
                           fromIntegral (fontUnitsPerEm fi `quot` 10),
      isFixedPitch = fromIntegral $ fromEnum $
                     fontMonospaced fi /// False,
      minMemType42 = 0,
      maxMemType42 = 0,
      minMemType1 = 0,
      maxMemType1 = 0,
      glyphNameIndex = [],
      postStrings = []}
    (panose1, panose2, panose3, panose4, panose5,
     panose6, panose7, panose8, panose9, panose10) =
      fontPanose fi /// (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    vendorID = case fontVendorID fi of
      Nothing -> 0x20202020
      Just (a, b, c, d) -> fromIntegral $
        ord a `shift` 24 .|. ord b `shift` 16 .|.
        ord c `shift` 8  .|. ord d
    selectionFlags = makeFlag
      [fontSlant fi == Italic,
       Underscore `elem` fontDecoration fi,
       Negative `elem` fontDecoration fi,
       Outlined `elem` fontDecoration fi,
       StrikeOut `elem` fontDecoration fi,
       fontWeight fi > NormalWeight,
       fontWeight fi == NormalWeight &&
       fontWidth fi == MediumWidth &&
       fontSlant fi == NoSlant &&
       null (fontSubFamilyExtra fi),
       True,
       null (fontSubFamilyExtra fi),
       fontSlant fi == Oblique]
    os2Tbl = OS2Table {
      os2version = 5,
      xAvgCharWidth = 0,
      usWeightClass = weightClass $ fontWeight fi,
      usWidthClass = widthClass $ fontWidth fi,
      fsType = embeddedBits $ fontEmbeddingLicence fi /// [],
      ySubscriptXSize = (fst <$> fontSubScriptSize fi) ///
                        (fontUnitsPerEm fi `quot` 2),
      ySubscriptYSize = (snd <$> fontSubScriptSize fi) ///
                        (fontUnitsPerEm fi `quot` 2),
      ySubscriptXOffset = (fst <$> fontSubScriptOffset fi) /// 0,
      ySubscriptYOffset = (snd <$> fontSubScriptOffset fi) ///
                          ((fontUnitsPerEm fi - fontEmBase fi) `quot` 2),
      ySuperscriptXSize = (fst <$> fontSuperScriptSize fi) ///
                          (fontUnitsPerEm fi `quot` 2),
      ySuperscriptYSize = (snd <$> fontSuperScriptSize fi) ///
                          (fontUnitsPerEm fi `quot` 2),
      ySuperscriptXOffset = (fst <$> fontSuperScriptOffset fi) /// 0,
      ySuperscriptYOffset = (snd <$> fontSuperScriptOffset fi) ///
                            (- (fontUnitsPerEm fi `quot` 4)),
      yStrikeoutSize = fontStrikoutSize fi ///
                       fromIntegral (fontUnitsPerEm fi `quot` 20),
      yStrikeoutPosition = fontStrikeoutPosition fi ///
                           fromIntegral (fontUnitsPerEm fi*10 `quot` 55),
      bFamilyClass = fromIntegral $
                     ((\(x,y) -> (x `shift` 8 .|. y)) <$> fontFamilyClass fi)
                     /// 0,
      bFamilyType = fromIntegral panose1,
      bSerifStyle = fromIntegral panose2,
      bWeight = fromIntegral panose3,
      bProportion = fromIntegral panose4, 
      bContrast = fromIntegral panose5,
      bStrokeVariation = fromIntegral panose6,
      bArmStyle = fromIntegral panose7,
      bLetterform = fromIntegral panose8,
      bMidline = fromIntegral panose9,
      bXHeight = fromIntegral panose10,
      ulUnicodeRange1 = (view _1 <$> fontUnicodeRanges fi) /// 3,
      ulUnicodeRange2 = (view _2 <$> fontUnicodeRanges fi) /// 0,
      ulUnicodeRange3 = (view _3 <$> fontUnicodeRanges fi) /// 0,
      ulUnicodeRange4 = (view _4 <$> fontUnicodeRanges fi) /// 0,
      achVendID = vendorID,
      fsSelection = selectionFlags,
      usFirstCharIndex = 0,
      usLastCharIndex = 0,
      sTypoAscender = fontUnitsPerEm fi - fontEmBase fi,
      sTypoDescender = fromIntegral $ - fontEmBase fi,
      sTypoLineGap = fontLineGap fi,
      usWinAscent = 0,
      usWinDescent = 0,
      ulCodePageRange1 = (fst <$> fontCodepageRanges fi) /// 0,
      ulCodePageRange2 = (snd <$> fontCodepageRanges fi) /// 0,
      sxHeight = 0,
      sCapHeight = fontCapHeight fi /// 0,
      usDefaultChar = 0,
      usBreakChar = 0,
      usMaxContext = 1,
      usLowerOpticalPointSize = fromIntegral $
        fontLowerOpticalPointSize fi /// 0,
      usUpperOpticalPointSize = fromIntegral $
        fontUpperOpticalPointSize fi /// 0xffff}
