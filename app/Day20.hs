{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

import           Math.NumberTheory.ArithmeticFunctions
import qualified Data.Set as S

search :: (Int -> Int -> Bool) -> Int -> Int -> Int
search stop input !n
  | stop n input = n
  | otherwise    = search stop input (n+1)

day20a :: Int -> Int
day20a input = search f (input `div` 10) 1
  where
    f n i = sigma 1 n >= i

day20b :: Int -> Int
day20b input = search f (input `div` 11) 1
  where
    f n i = S.foldl' (\acc d -> if 50*d >= n then acc+d else acc) 0 (divisors n) >= i

main ::IO ()
main = do 
  let input = 33100000
  print $ day20a input
  print $ day20b input
