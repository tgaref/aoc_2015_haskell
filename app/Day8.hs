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
import           Control.Foldl (Fold)
import qualified Control.Foldl as F

readInput :: FilePath -> IO [Text]
readInput file = fmap (removeEnd . removeStart . T.strip) . T.lines <$> readFileText file
  where
    removeStart t = case T.uncons t of
      Just (_,as) -> as
      Nothing     -> error "This should not happen!"
    removeEnd t = case T.unsnoc t of
      Just (as,_) -> as
      Nothing     -> error "This should not happen!"

memoryLength :: Text -> Int
memoryLength s =
  T.length s - 3*unis - slashes - quotes
    where
      (_, slashes, t) = removeSlash s 
      unis = T.count "\\x" t
      quotes = T.count "\\\"" t

removeSlash :: Text -> (Bool, Int, Text)
removeSlash =
  T.foldl' (\(prev, count, str) c -> if | c == '\\' && prev     -> (False, count+1, str)
                                        | c == '\\' && not prev -> (True, count, str)
                                        | c /= '\\' && prev     -> (False, count, T.snoc (T.snoc str '\\') c)
                                        | c /= '\\' && not prev -> (False, count, T.snoc str c)
                                        | otherwise             -> error "This is not possible!"  
           ) (False, 0, "")

codeLength :: Text -> Int
codeLength t = 2 + T.length t

lenDiffF :: Fold Text Int
lenDiffF = F.Fold step 0 identity
  where
    step acc t = acc + codeLength t - memoryLength t

countEsc :: Text -> Int
countEsc t = T.count "\\" t + T.count "\"" t

lenIncF :: Fold Text Int
lenIncF = F.Fold step 0 identity
  where
    step acc t = acc + countEsc t + 4

day8a :: IO Int
day8a =
  F.fold lenDiffF <$> readInput "inputs/8.input"

day8b :: IO Int
day8b = do
  F.fold lenIncF <$> readInput "inputs/8.input"

main ::IO ()
main = do
  day8a >>= print
  day8b >>= print
