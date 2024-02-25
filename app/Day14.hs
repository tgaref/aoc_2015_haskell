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
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as M

data Reindeer = R !Int !Int !Int
  deriving stock (Eq, Ord, Show)

readInput :: FilePath -> IO (Map Text Reindeer)
readInput file = M.fromList . fmap parseLine . T.lines <$> readFileText file

parseLine :: Text -> (Text, Reindeer)
parseLine t = case A.parseOnly lineP t of
  Left _  -> error "Error parsing input"
  Right a -> a

nameP :: Parser Text
nameP = A.string "Dancer" <|> A.string "Cupid" <|> A.string "Rudolph"
  <|> A.string "Donner" <|> A.string "Dasher" <|> A.string "Blitzen"
  <|> A.string "Prancer" <|> A.string "Comet" <|> A.string "Vixen"

reindeerP :: Parser Reindeer
reindeerP = R <$> (A.string " can fly " *> A.decimal)
  <*> (A.string " km/s for " *> A.decimal)
  <*> (A.string " seconds, but then must rest for " *> A.decimal) 

lineP :: Parser (Text, Reindeer)
lineP = (,) <$> nameP <*> (reindeerP <* A.takeText)

distanceTraveled :: Int -> Reindeer -> Int
distanceTraveled t (R speed runTime restTime) = (a*runTime + b) * speed
  where
     (a,r) = t `quotRem` (runTime + restTime)
     b = min runTime r 

atLead :: Int -> Map Text Reindeer -> (Int, [Text])
atLead t m = M.foldlWithKey' (\(d, ws) name reindeer ->
                                 let d' = distanceTraveled t reindeer
                                 in if | d' == d -> (d, name:ws)
                                       | d' > d  -> (d', [name])
                                       | d' < d  -> (d, ws)
                             ) (0,[]) m
         
day14a :: Int -> Map Text Reindeer -> Int
day14a time m = fst $ atLead time m
    
day14b :: Int -> Map Text Reindeer -> Int
day14b time m = M.foldl' max 0 scoreBoard
  where
    score = M.fromList [("Dancer",0),("Cupid",0),("Rudolph",0),("Donner",0),("Dasher",0),("Blitzen",0),("Prancer",0),("Comet",0),("Vixen",0)]
    record s ws = foldl' (\acc name -> M.insertWith (+) name 1 acc) s ws
    scoreBoard = foldl' (\s t -> record s (snd $ atLead t m)) score [1..time]  

main ::IO ()
main = do
  m <- readInput "inputs/14.input"
  print $ day14a 2503 m
  print $ day14b 2503 m 

