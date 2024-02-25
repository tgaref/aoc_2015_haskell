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
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.List as L

readInput :: FilePath -> IO [Text]
readInput file = T.lines <$> readFileText file

newtype Location = L Text
  deriving newtype (Eq, Ord, Show)

townP :: Parser Location
townP = L . T.pack <$> A.many1 A.letter

lineP :: Parser (Location, Location, Int)
lineP = (,,) <$> (townP <* A.string " to ") <*> (townP <* A.string " = ") <*> A.decimal

order :: Ord a => (a,a) -> (a,a)
order (a,b) = if a <= b then (a,b) else (b,a) 

parseLine :: Text -> ((Location, Location), Int)
parseLine t = case A.parseOnly lineP t of
  Right (l1,l2,d) -> (order (l1,l2),d) 
  Left _          -> error "Failed to parse command..."

parseInput :: IO (Map (Location, Location) Int, Set Location)
parseInput = do
  input <- fmap parseLine <$> readInput "inputs/9.input"
  let locations = foldl' (\acc ((l1, l2), d) -> Set.insert l2 (Set.insert l1 acc)) Set.empty input
  pure $ (M.fromList input, locations)

routeCost :: (Ord a, Num a) => [Location] -> Reader (Map (Location, Location) a) a
routeCost locations = do
  costs <- ask
  let (la : locations') = locations 
  let distance a b = case M.lookup (order (a,b)) costs of
        Just d  -> d
        Nothing -> error $ "Pair " <> show a <> " and " <> show b <> " don't have a defined distance."
  pure $ fst $ foldl' (\(dist, a) b -> (dist + distance a b, b)) (0, la) locations'      

computeBy ::(Ord a, Num a) => ([a] -> a) -> Reader (Map (Location, Location) a, [Location]) a
computeBy f = do
  (m, locations) <- ask
  pure $ f [runReader (routeCost p) m | p <- permutations locations]

day9a :: (Ord a, Num a) => Reader (Map (Location, Location) a, [Location]) a
day9a = computeBy L.minimum

day9b :: (Ord a, Num a) => Reader (Map (Location, Location) a, [Location]) a
day9b = computeBy L.maximum

main ::IO ()
main = do
  (m, locations) <- parseInput
  print $ runReader day9a (m, Set.toList locations)
  print $ runReader day9b (m, Set.toList locations)
