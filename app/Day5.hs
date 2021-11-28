{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

import Lib
import qualified Data.Text as T

readInput :: IO [Text]
readInput = T.lines <$> readFileText "inputs/5.input"
  

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel  _  = False

vowelTest :: Text -> Bool
vowelTest w = T.length (T.filter isVowel w) >= 3

twiceTest :: Text -> Bool
twiceTest w = snd $ T.foldl' (\(prev, found) current -> (current, found || prev == current)) (' ', False) w

forbiddenTest :: Text -> Bool
forbiddenTest w =
  length (T.splitOn "ab" w) == 1 &&
  length (T.splitOn "cd" w) == 1 &&
  length (T.splitOn "pq" w) == 1 &&
  length (T.splitOn "xy" w) == 1 

containsTwice :: Text -> Text -> Bool
containsTwice w x = length (T.splitOn x w) > 2

containsTwiceTest :: Text -> Bool
containsTwiceTest w = or $ fmap (w `containsTwice`) (substrOflen w 2)

inBetween :: Text -> Bool
inBetween w = T.length w == 3 && T.head w == T.last w
                      
inBetweenTest :: Text -> Bool
inBetweenTest w = or $ fmap inBetween (substrOflen w 3) 
  
isNice :: [Text -> Bool] -> Text -> Bool
isNice preds w = and [f w | f <- preds]

day5a :: IO Int
day5a = length . filter (isNice [vowelTest, twiceTest, forbiddenTest]) <$> readInput
  
day5b :: IO Int
day5b = length . filter (isNice [containsTwiceTest, inBetweenTest]) <$> readInput

main ::IO ()
main = do
  day5a >>= print
  day5b >>= print
