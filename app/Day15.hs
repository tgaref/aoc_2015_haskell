{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where


import           Data.List (maximum)

data Properties = P !Int !Int !Int !Int !Int
  deriving stock (Eq, Ord, Show)

--         "Sugar"    :   3   0  0    (-3) 2
--         "Sprinkles":  (-3) 3  0    0    9
--         "Candy"    :  (-1) 0  4    0    1
--         "Chocolate":   0   0  (-2) 2    8
--        

scoreA :: (Int, Int, Int, Int) -> Int
scoreA (a,b,c,d) = capacity * duration * flavor * texture
    where
      capacity = max 0 (3*a -3*b -c)
      duration = max 0 (3*b)
      flavor   = max 0 (4*c-2*d)
      texture  = max 0 (-3*a + 2*d)

scoreB :: (Int, Int, Int, Int) -> Int
scoreB (a,b,c,d) = if calories == 500
  then capacity * duration * flavor * texture
  else 0
    where
      capacity = max 0 (3*a -3*b -c)
      duration = max 0 (3*b)
      flavor   = max 0 (4*c-2*d)
      texture  = max 0 (-3*a + 2*d)
      calories = max 0 (2*a + 9*b + c + 8*d)

day15a :: Int
day15a = maximum [ scoreA (sugar, sprink, candy, 100 - sugar - sprink - candy) | sugar <- [0 .. 100]
                                                                              , sprink <- [0 .. 100 - sugar]
                                                                              , candy <- [0 .. 100 - sugar - sprink]]
day15b :: Int
day15b = maximum [ scoreB (sugar, sprink, candy, 100 - sugar - sprink - candy) | sugar <- [0 .. 100]
                                                                              , sprink <- [0 .. 100 - sugar]
                                                                              , candy <- [0 .. 100 - sugar - sprink]]
         
main ::IO ()
main = do
  print day15a
  print day15b

