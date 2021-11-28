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
import qualified Data.Set as S
import Control.Foldl (Fold)
import qualified Control.Foldl as F

data Position = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

locations :: Fold Char (Position, Set Position)
locations = F.Fold step (Pos 0 0, S.singleton (Pos 0 0)) identity
 where
   step (Pos x y, ps) '>' = let p = Pos (x+1) y in (p, S.insert p ps)
   step (Pos x y, ps) '<' = let p = Pos (x-1) y in (p, S.insert p ps)
   step (Pos x y, ps) '^' = let p = Pos x (y+1) in (p, S.insert p ps)
   step (Pos x y, ps) 'v' = let p = Pos x (y-1) in (p, S.insert p ps) 
   step _              _  = error "Undefined direction!"

partition :: [a] -> ([a], [a])
partition directions = go directions [] []
  where
    go []         s1 s2 = (reverse s1, reverse s2)
    go [x]        s1 s2 = (x:s1, s2)
    go (x:y:rest) s1 s2 = go rest (x:s1) (y:s2)

day3a :: IO Int
day3a = do
  directions <- readFileText "inputs/3.input"
  pure $ S.size $ snd $ F.fold locations (T.unpack directions)
  
day3b :: IO Int
day3b = do
  directions <- readFileText "inputs/3.input"
  let (dir1, dir2) = partition (T.unpack directions)
  let set1 = snd $ F.fold locations dir1
  let set2 = snd $ F.fold locations dir2
  pure $ S.size $ S.union set1 set2

main ::IO ()
main = do
  day3a >>= print
  day3b >>= print
  
