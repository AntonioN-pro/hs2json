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

fsep :: [Doc] -> Doc
fsep _xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
