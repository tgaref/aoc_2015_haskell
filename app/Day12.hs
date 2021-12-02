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

import qualified Data.Aeson as A
import Data.Aeson.KeyMap (KeyMap)
import           Data.Scientific (coefficient)

readInput :: IO A.Value
readInput = do
  input <- readFileBS "inputs/12.input"
  pure $ case A.decodeStrict input :: Maybe A.Value of
           Just a -> a
           Nothing -> error "Wrong JSON value"

evalJa :: A.Value -> Integer 
evalJa A.Null       = 0
evalJa (A.Number a) = coefficient a
evalJa (A.String _) = 0
evalJa (A.Bool _)   = 0 
evalJa (A.Array xs) = sum $ evalJa <$> xs
evalJa (A.Object m) = foldl' (\acc v -> acc + evalJa v) 0 m 

evalJb :: A.Value -> Integer 
evalJb A.Null       = 0
evalJb (A.Number a) = coefficient a
evalJb (A.String _) = 0
evalJb (A.Bool _)   = 0 
evalJb (A.Array xs) = sum $ evalJb <$> xs
evalJb (A.Object m)
  | hasVal (A.String "red") m = 0 
  | otherwise = foldl' (\acc v -> acc + evalJb v) 0 m

hasVal :: A.Value -> KeyMap A.Value -> Bool
hasVal val = foldl' (\acc v -> (v == val) || acc) False

day12a :: A.Value -> Integer
day12a = evalJa

day12b :: A.Value -> Integer
day12b = evalJb

main ::IO ()
main = do
  input <- readInput
  print $ day12a input
  print $ day12b input
