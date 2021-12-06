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

import qualified Data.Text as T
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as M

type Aunt = Map Compound Int 

data Compound = Children
              | Cats
              | Samoyeds
              | Pomeranians
              | Akitas
              | Vizslas
              | Goldfish
              | Trees
              | Cars
              | Perfumes
              deriving stock (Eq, Ord, Bounded, Show)

readInput :: FilePath -> IO [Aunt]
readInput file = fmap parseLine . T.lines <$> readFileText file

parseLine :: Text -> Aunt
parseLine t = case A.parseOnly lineP t of
  Left _  -> error "Failed to parse Aunt..."
  Right a -> a 

compoundP :: Parser Compound
compoundP = A.string "children" $> Children
        <|> A.string "cats" $> Cats
        <|> A.string "samoyeds" $> Samoyeds
        <|> A.string "pomeranians" $> Pomeranians
        <|> A.string "akitas" $> Akitas
        <|> A.string "vizslas" $> Vizslas
        <|> A.string "goldfish" $> Goldfish
        <|> A.string "trees" $> Trees
        <|> A.string "cars" $> Cars
        <|> A.string "perfumes" $> Perfumes

numberP :: Parser Int
numberP = A.string ": " *> A.decimal

itemP :: Parser (Compound, Int)
itemP = (,) <$> compoundP <*> numberP

lineP :: Parser Aunt
lineP = do
  A.skipWhile (/= ':')
  A.skip (==':')
  A.skipSpace
  M.fromList <$> itemP `A.sepBy'` (A.string ", ")   

target :: Map Compound Int
target = M.fromList [ (Children, 3)
                    , (Cats, 7)
                    , (Samoyeds, 2)
                    , (Pomeranians, 3)
                    , (Akitas, 0)
                    , (Vizslas, 0)
                    , (Goldfish, 5)
                    , (Trees, 3)
                    , (Cars, 2)
                    , (Perfumes, 1)
                    ]

check :: (Aunt -> Compound -> Int -> Bool) -> Aunt -> Aunt -> Bool
check test t a = M.foldlWithKey' (\acc key val -> acc && test t key val)  True a

testA :: Aunt -> Compound -> Int -> Bool
testA t key val = t ! key == val

testB :: Aunt -> Compound -> Int -> Bool
testB t Cats val        = t ! Cats < val
testB t Trees val       = t ! Trees < val
testB t Pomeranians val = t ! Trees > val
testB t Goldfish val    = t ! Goldfish > val
testB t key val         = t ! key == val

findBy :: (Aunt -> Aunt -> Bool) -> [Aunt] -> (Int, Aunt)
findBy f aunts = go aunts 1
  where
    go []       _  = error "Did not find aunt..."
    go (a:rest) !k = if f target a
      then (k, a)
      else go rest (k+1) 

day16a :: [Aunt] -> (Int, Aunt)
day16a = findBy (check testA)

day16b :: [Aunt] -> (Int, Aunt)
day16b = findBy (check testB)
         
main ::IO ()
main = do
  input <- readInput "inputs/16.input"
  print $ day16a input
  print $ day16b input
