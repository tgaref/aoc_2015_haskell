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


day1a :: Text -> Int
day1a  = T.foldl' (\fl c -> if c == '(' then fl + 1 else fl - 1) 0

day1b :: Text -> Int
day1b input = go input 0 0
  where
    go :: Text -> Int -> Int -> Int 
    go text fl pos
     | text == "" = error "Did not go to basement!"
     | otherwise  =
      let fl' = fl + if T.head text == '(' then 1 else -1
      in
        if fl' == -1
        then pos + 1
        else go (T.tail text) fl' (pos+1)
            
main :: IO ()
main = do
  input <- readFileText "inputs/1.input"
  print $ day1a input
  print $ day1b input
  
