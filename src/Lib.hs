module Lib where

import qualified Data.Text as T

contains :: Text -> Text -> Bool
contains w x = or $ fmap (x `T.isPrefixOf`) (T.tails w)

substrOflen :: Text -> Int -> [Text]
substrOflen wrd n = go wrd []
  where
    go w acc
      | T.length w < n  = reverse acc
      | otherwise       = go (T.tail w) (T.take n w : acc)

simulate :: Int -> (a -> a) -> a -> a
simulate !n f !a
 | n <= 0    = a
 | otherwise = simulate (n-1) f (f a) 

rotateL :: Int -> [a] -> [a]
rotateL _ []     = []
rotateL n xs = drop n xs <> take n xs
