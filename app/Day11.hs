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

nextLetter :: Char -> Char
nextLetter 'h' = 'j'
nextLetter 'n' = 'p'
nextLetter 'k' = 'm'
nextLetter 'z' = 'a'
nextLetter c   = succ c
    
nextPass :: Text -> Text
nextPass t =
  if rest == "" then
    T.map (const 'a') zs
  else
    T.snoc b (nextLetter a) <> T.map (const 'a') zs  
  where    
    (zs, rest) = (T.takeWhileEnd (== 'z') t, T.dropWhileEnd (== 'z') t)
    (b, a)     = case T.unsnoc rest of
      Just p  -> p
      Nothing -> error "Should not happen!"

isValid :: Text -> Bool
isValid t = consecutive t && countPairs t >= 2
  where    
    inSeq t
      | T.length t == 0 = True
      | otherwise       =
          let (h,rest) = case T.uncons t of
                Nothing -> error "Should not happen!"
                Just p  -> p 
          in snd (T.foldl' (\(prev, n) c -> (c, if c == succ prev then n+1 else 0)) (h, 0 :: Int) rest) >= 2
    consecutive t
      | T.length t < 3 = False
      | otherwise      =
          let pref = T.take 3 t
          in inSeq pref || consecutive (T.drop 1 t) 
    countPairs :: Text -> Int
    countPairs t =
      fst $ T.foldl' (\(count, prev) c -> if prev == Just c
                       then (count+1, Nothing)
                       else (count, Just c)
                     ) (0, Nothing) t 

nextValidPass :: Text -> Text
nextValidPass t = go $ nextPass t
  where
    go t
      | isValid t = t
      | otherwise = go (nextPass t) 
    
day11a :: Text -> Text 
day11a = nextValidPass

day11b :: Text -> Text
day11b = nextValidPass . day11a

main ::IO ()
main = do
  print $ day11a "cqjxjnds"
  print $ day11b "cqjxjnds"
