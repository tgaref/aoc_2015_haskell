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

import Lib (rotateL)
import qualified Data.Text as T
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as M
import           Data.List (maximum, minimum)

readInput :: IO (Map (Text, Text) Int, [Text])
readInput = do
  input <- fmap (change . A.parseOnly lineP) . T.lines <$> readFileText "inputs/13.input"
  let m = foldl' (\acc ((a,b),v) -> M.insertWith (+) (order (a,b)) v acc) M.empty input
  let ps = ordNub $ foldl' (\acc ((p,_),_) -> p:acc) [] input 
  pure $ (m,ps)
    where
      change :: Either string (Text, Int, Int, Text) -> ((Text, Text), Int)
      change (Right (p1, h, sign, p2)) = ((p1,p2), sign*h)
      change (Left _)  = error "Failed to parse line..."

nameP :: Parser Text
nameP = A.string "Alice" <|> A.string "Bob" <|> A.string "Carol"
  <|> A.string "David" <|> A.string "Eric" <|> A.string "Frank"
  <|> A.string "George" <|> A.string "Mallory"

happyP :: Parser Int
happyP = A.decimal

gainP :: Parser Int
gainP = A.string " would gain " $> 1

loseP :: Parser Int 
loseP = A.string " would lose " $> -1

lineP :: Parser (Text, Int, Int, Text)
lineP = (,,,) <$> nameP
  <*> (gainP <|> loseP)
  <*> (happyP <* A.string " happiness units by sitting next to ")
  <*> (nameP <* A.string ".")

happynessA :: [Text] -> Reader (Map (Text, Text) Int) Int
happynessA ps = do
  m <- ask
  let pairs = order <$> zip ps (rotateL 1 ps)
  pure $ sum $ mapMaybe (`M.lookup` m) pairs

order :: Ord a => (a,a) -> (a,a)
order (a,b) = min (a,b) (b,a)

happynessB :: [Text] -> Reader (Map (Text, Text) Int) Int
happynessB ps = do
  m <- ask
  let pairs = order <$> zip ps (rotateL 1 ps)
  let happyList = mapMaybe (`M.lookup` m) pairs 
  pure $ sum happyList - minimum happyList 
    
day13a :: Map (Text, Text) Int -> [Text] -> Int
day13a m ppl = maximum [ runReader (happynessA ps) m | ps <- permutations ppl]

day13b :: Map (Text, Text) Int -> [Text] -> Int
day13b m ppl = maximum [ runReader (happynessB ps) m | ps <- permutations ppl]

main ::IO ()
main = do
  (m, persons) <- readInput
  print $ day13a m persons
  print $ day13b m persons
