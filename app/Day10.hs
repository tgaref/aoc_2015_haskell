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

import Lib (simulate)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as B

step :: LText -> LText
step t = go t mempty
  where
    go "" acc = B.toLazyText acc
    go t  acc =      
      let h = T.head t
          (a,t') = (T.takeWhile (== h) t, T.dropWhile (== h) t) 
          acc' = acc <> B.fromText (show (T.length a)) <> B.singleton h
      in go t' acc' 

day10a :: LText -> Int64
day10a input = T.length $ simulate 40 step input

day10b :: LText -> Int64 
day10b input = T.length $ simulate 50 step input

main ::IO ()
main = do
  print $ day10a "1113122113"
  print $ day10b "1113122113"
