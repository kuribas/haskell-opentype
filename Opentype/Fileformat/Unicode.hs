-- | Utilities for making it easier to create Tables from Unicode
-- data.
module Opentype.Fileformat.Unicode
  (-- * Unicode tables
    UnicodeGlyphMap, UnicodeKernPairs, makeUnicodeTables,
    makeUnicodeFont, 
    -- * Postscript Names
    module Opentype.Fileformat.Unicode.PostNames,
  )

where
import Opentype.Fileformat.Unicode.PostNames
import Opentype.Fileformat
import Opentype.Fileformat.FontInfo
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.IntSet as IS (empty)
import Data.List (sortBy)
import Data.Either
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import Data.Function

-- | A map from glyphnames to glyphs.  Unicode points are derived
-- using `nameToCodepoint`.  Names without matching codepoint can only
-- be used in composite glyphs.  If the `glyphName` has no matching
-- key in the map, it will be substituted by the empty glyph
type UnicodeGlyphMap = HM.HashMap String (Glyph String)
type UnicodeKernPairs = [(String, String, FWord)]

-- | Create an opentype font from a `UnicodeGlyphMap` and `FontInfo`
-- structure.
makeUnicodeFont :: UnicodeGlyphMap -> UnicodeKernPairs -> FontInfo -> OpentypeFont
makeUnicodeFont uniGlyphs kernPrs info =
  OpentypeFont False headTbl hheaTbl cmapTbl
  nameTbl postTbl (Just os2Tbl) (if null (kernPairs kernTbl) then Nothing else Just kernTbl)
  (QuadTables emptyMaxpTable glyfTbl) M.empty
  where
    (headTbl, hheaTbl, nameTbl, postTbl', os2Tbl) =
      infoToTables info
    (cmapTbl, glyfTbl, postTbl, kernTbl) =
      makeUnicodeTables uniGlyphs kernPrs postTbl'
  

-- | Given a `UnicodeGlyphMap`, create a `CmapTable`, `GlyfTable` and
-- `KernTable` and update the `PostTable`.
makeUnicodeTables :: UnicodeGlyphMap -> UnicodeKernPairs -> PostTable -> (CmapTable, GlyfTable, PostTable, KernTable)
makeUnicodeTables uniGlyphs kernPrs postTbl =
  (cmapTbl, GlyfTable glyphVec,
   postTbl {postVersion = PostTable2,
            glyphNameIndex = postscriptNameIndex,
            postStrings = postscriptExtraNames},
   KernTable 1 realKernPairs)
  where
    uniqueCodepoints, codepoints :: [(String, (Int, Glyph String))]
    glyphComps :: [(String, Glyph String)]
    uniqueCodepoints =
      filter (\(s, (_, g)) -> glyphName g == s)
      codepoints
    (codepoints, glyphComps) =
      partitionEithers $ map getCodePoint $
      HM.toList uniGlyphs
    nameMap :: HM.HashMap String GlyphID
    nameMap =
      HM.fromList $ flip zip [1..] $
      map fst (sortBy (compare `on` (fst.snd)) uniqueCodepoints)
      ++ map fst glyphComps
    postscriptNameMap :: HM.HashMap String Int
    postscriptNameMap =
      HM.fromList $ zip postscriptExtraNames [258..]
    postscriptExtraNames :: [String]
    postscriptExtraNames =
      filter (isNothing . postscriptIndex) $
      map fst uniqueCodepoints
    postscriptNameIndex :: [Int]
    postscriptNameIndex =
      flip map uniqueCodepoints $ \(n, _) ->
      fromMaybe 0 $
      HM.lookup n postscriptNameMap
      <|> postscriptIndex n
    glyphVec :: V.Vector StandardGlyph
    glyphVec =
      V.fromList $
      map (normalizeGlyph.snd.snd) uniqueCodepoints ++
      map (normalizeGlyph.snd) glyphComps
    codepointMap :: WordMap GlyphID
    codepointMap =
      M.fromList $
      flip map codepoints $ \(_, (code, g)) ->
      (fromIntegral code,
       fromMaybe 0 $ HM.lookup (glyphName g) nameMap)
    normalizeGlyph :: Glyph String -> StandardGlyph
    normalizeGlyph =
      fmap (fromIntegral . fromMaybe 0 . (`HM.lookup` nameMap))
    getCodePoint (name, g) =
      case nameToCodepoint name of
        Nothing -> Right (name, g)
        Just cp -> Left (name, (cp, g))
    cmapTbl = CmapTable $ bmpCmap : fullCmap
    bmpCmap = CMap MicrosoftPlatform 1 0 MapFormat4 IS.empty codepointMap
    fullCmap
      | M.null codepointMap ||
        fst (M.findMax codepointMap) <= 0xffff = []
      | otherwise =
        [CMap MicrosoftPlatform 10 0
         MapFormat12 IS.empty codepointMap]
    realKernPairs = flip mapMaybe kernPrs $ \(s1, s2, x) -> do
      c1 <- HM.lookup s1 nameMap
      c2 <- HM.lookup s2 nameMap
      return $ KernPair c1 c2 x
    
