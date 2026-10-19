module Prettify where

import Prelude hiding ((<>))

data Doc = ToBeDefined
         deriving (Show)

text :: String -> Doc
text _str = undefined

double :: Double -> Doc
double _num = undefined

(<>) :: Doc -> Doc -> Doc
_a <> _b = undefined

char :: Char -> Doc
char _c = undefined

hcat :: [Doc] -> Doc
hcat _xs = undefined
