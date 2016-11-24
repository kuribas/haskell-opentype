module Opentype.Fileformat.FontInfo where

data Language = English | Unicode

newtype InternatString = InternatString [(Language, String)]
