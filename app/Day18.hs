{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

import           Lib (simulate)
import qualified Data.Text as T
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

type Grid a = Vector (Vector a)
data Coords =  C {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

cell :: Grid a -> Coords -> a
cell grid (C i j) = (grid ! j) ! i

gridSize :: Grid a -> (Int, Int)
gridSize grid = (V.length (grid ! 0) - 1, V.length grid - 1)

map2 :: (a -> b) -> Grid a -> Grid b
map2 f = V.map (V.map f)

readInput :: FilePath -> IO (Grid Bool)
readInput file = V.fromList . fmap readBool . T.lines <$> readFileText file

readBool :: Text -> Vector Bool
readBool t = V.fromList $ reverse $ T.foldl' (\acc c -> if c == '#' then True:acc else False:acc) [] t

step :: (Grid Bool -> Coords -> Bool -> Int -> Bool) -> Grid Bool -> Grid Bool
step f grid = V.imap (\j row -> V.imap (\i v -> let ns = filter (cell grid) $ neighbours grid (C i j)
                                                in f grid (C i j) v (length ns)) row) grid
    
criterionA :: Grid Bool -> Coords -> Bool -> Int -> Bool    
criterionA _ _ True ns
  | ns == 2 || ns == 3  = True
  | otherwise           = False
criterionA _ _ False ns
  | ns == 3   = True
  | otherwise = False

criterionB :: Grid Bool -> Coords -> Bool -> Int -> Bool    
criterionB grid (C i j) True ns
  | (i == 0 || i == x) && (j == 0 || j == y) = True
  | ns == 2 || ns == 3  = True
  | otherwise           = False
  where
    (x,y) = gridSize grid
criterionB _ (C i j) False ns
  | ns == 3   = True
  | otherwise = False

initialize :: Grid Bool -> Grid Bool
initialize grid = V.imap (\j row -> V.imap (\i v -> ((i == 0 || i == x) && (j == 0 || j == y)) || v) row) grid
  where
    (x,y) = gridSize grid
    
neighbours :: Grid Bool -> Coords -> [Coords]  
neighbours grid (C i j) =
  filter (\(C a b) -> (a >= 0) && (a <= x) && (b >= 0) && (b <= y)) [ C (i-1) (j-1), C i (j-1), C (i+1) (j-1), C (i-1) j
  , C (i+1) j, C (i-1) (j+1), C i (j+1), C (i+1) (j+1)]      
  where
    (x,y) = gridSize grid
    
day18a :: Grid Bool -> Int
day18a grid = sum $ V.map sum $  map2 (\v -> if v then 1 else 0) finalGrid
  where
    finalGrid = simulate 100 (step criterionA) grid

day18b :: Grid Bool -> Int
day18b grid = sum $ V.map sum $  map2 (\v -> if v then 1 else 0) finalGrid
  where
    finalGrid = simulate 100 (step criterionB) $ initialize grid
         
main ::IO ()
main = do 
  grid <- readInput "inputs/18.input"
  print $ day18a grid
  print $ day18b grid
