{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.IntSet as IS
import qualified Data.Set as S

-- Read input

readInput :: FilePath -> IO [Int]
readInput file = fmap readInt . T.lines <$> readFileText file

readInt :: Text -> Int
readInt t = case T.decimal t of
  Left _       -> error "Failed to read int"
  Right (i, _) -> i 

-- Solution

insertNumber :: Int -> Int -> Set (IntSet) -> Set (IntSet)
insertNumber k n sets = S.filter (\s -> IS.size s == k) $ S.map (IS.insert n) sets

extend :: Int -> [Int] -> Set (IntSet) -> Set (IntSet)
extend k numbers sets = S.unions $ fmap (\n -> insertNumber k n sets) numbers

balance :: Int -> [Int] -> Int
balance into input = go 0 (S.singleton IS.empty)
  where
    w = (sum input) `div` into
    entanglement :: IntSet -> Int
    entanglement s = IS.foldl' (*) 1 s
    go :: Int -> Set IntSet -> Int
    go !k sets
      | S.null good = go (k+1) sets'
      | otherwise   = S.foldl' (\acc s -> min acc (entanglement s)) (maxBound :: Int) good
      where
        good = S.filter (\s -> IS.foldl' (+) 0 s == w) sets
        sets' = extend (k+1) input sets 
  
day24a :: [Int] -> Int
day24a = balance 3

day24b :: [Int] -> Int
day24b = balance 4

main ::IO ()
main = do
  input <- readInput "inputs/24.input"
  print $ day24a input
  print $ day24b input
