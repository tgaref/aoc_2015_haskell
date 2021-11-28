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

