{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

import Data.Mod

-- Solution

day25a :: Int -> Int -> Natural
day25a i j = unMod $ x1 * c ^% n
  where
    x1 = 20151125 :: Mod 33554393 
    c = 252533 :: Mod 33554393
    n = div (i*(i-1) + j*(j-1)) 2 + i * (j-1)
    
main ::IO ()
main = print $ day25a 3010 3019
