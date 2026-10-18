module Prettify where

import Prelude hiding ((<>))

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string _str = undefined

text :: String -> Doc
text _str = undefined

double :: Double -> Doc
double _num = undefined
