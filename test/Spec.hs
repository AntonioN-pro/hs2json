{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missed-extra-shared-lib #-}

module Main (main) where

import Prelude hiding ((<>))

import Test.QuickCheck
import Data.List (intersperse)
import Control.Monad (liftM, liftM2)
import System.Exit (exitFailure)

import SimpleJSON
import Prettify

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

-- | Gera um JValue recursivo limitando tanto a profundidade (via sized)
-- quanto a largura das listas internas (via resize (n `div` 2) envolvendo listOf),
-- evitando explosao exponencial de memoria em JObject e JArray.
instance Arbitrary JValue where
    arbitrary = sized genJValue

genJValue :: Int -> Gen JValue
genJValue n
  | n <= 0    = leaf
  | otherwise = oneof [leaf, recursive]
  where
    leaf = oneof
      [ JString <$> arbitrary
      , JNumber <$> arbitrary
      , JBool   <$> arbitrary
      , pure JNull
      ]
    recursive = oneof
      [ JObject <$> resize (n `div` 2) (listOf field)
      , JArray  <$> resize (n `div` 2) (listOf smaller)
      ]
    field   = (,) <$> arbitrary <*> smaller
    smaller = genJValue (n `div` 2)

prop_empty_id :: Doc -> Bool
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char :: Char -> Bool
prop_char c = char c == Char c

prop_text :: String -> Bool
prop_text s = text s == if null s then Empty else Text s

prop_line :: Bool
prop_line = line == Line

prop_double :: Double -> Bool
prop_double d = double d == text (show d)

prop_hcat :: [Doc] -> Bool
prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

prop_punctuate' :: Doc -> [Doc] -> Bool
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]
        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys

prop_compact_text :: String -> Bool
prop_compact_text s = compact (text s) == s

prop_pretty_text :: Positive Int -> String -> Bool
prop_pretty_text (Positive w) s = pretty w (text s) == s

prop_getString_roundtrip :: String -> Bool
prop_getString_roundtrip s = getString (JString s) == Just s

prop_getInt_truncate :: Double -> Bool
prop_getInt_truncate d = getInt (JNumber d) == Just (truncate d)

prop_getDouble_roundtrip :: Double -> Bool
prop_getDouble_roundtrip d = getDouble (JNumber d) == Just d

prop_getBool_roundtrip :: Bool -> Bool
prop_getBool_roundtrip b = getBool (JBool b) == Just b

prop_getObject_roundtrip :: [(String, JValue)] -> Bool
prop_getObject_roundtrip o = getObject (JObject o) == Just o

prop_getArray_roundtrip :: [JValue] -> Bool
prop_getArray_roundtrip a = getArray (JArray a) == Just a

prop_isNull_correct :: JValue -> Bool
prop_isNull_correct v = isNull v == (v == JNull)

prop_accessors_safe_on_mismatch :: String -> Double -> Bool -> Bool
prop_accessors_safe_on_mismatch s d b =
    getString (JNumber d) == Nothing
 && getDouble (JString s) == Nothing
 && getBool   JNull       == Nothing
 && getObject (JBool b)   == Nothing
 && getArray  (JString s) == Nothing

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    passed <- runTests
    if passed
        then putStrLn "Passou em todos os testes (Prettify + JValue)."
        else do putStrLn "Alguns testes falharam."
                exitFailure
