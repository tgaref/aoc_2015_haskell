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
import qualified Data.List as L
  
readInt :: Text -> Int
readInt t = case T.decimal t of
  Left _       -> error "Failed to read int"
  Right (i, _) -> i 

readInput :: FilePath -> IO [Int]
readInput file = fmap readInt . T.lines <$> readFileText file

day17a :: [Int] -> Int
day17a sizes = length $ filter (== 150) $ sum <$> subsets
 where
   subsets = L.subsequences sizes

day17b :: [Int] -> Int
day17b sizes = length $ filter (\(l,_) -> l == minL) good 
 where
   subsets = L.subsequences sizes
   good = sortOn fst $ filter (\(_,s) -> s == 150) $ fmap (\s -> (length s, sum s)) subsets
   minL = case good of
     []     -> error "No solution..."
     ((l,_):_) -> l
         
main ::IO ()
main = do
  input <- readInput "inputs/17.input"
  print $ day17a input
  print $ day17b input
