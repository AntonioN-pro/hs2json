{-# OPTIONS_GHC -Wno-x-partial #-}
module QuickTestes where

import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum :: Ord a => [a] -> Bool
prop_minimum xs = head (qsort xs) == minimum xs

prop_minimum' :: Ord a => [a] -> Property
prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs
