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
import GHC.OldList (maximum)

data Size = Size {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

inputParser :: Parser Size
inputParser = Size <$> A.decimal <* A.char 'x' <*> A.decimal <* A.char 'x' <*> A.decimal

parseLine :: Text -> Size
parseLine line = case A.parseOnly inputParser line of
  Right a -> a
  Left  _ -> error "Problem parsing line"

calculateTotal :: (Size -> Int) -> [Size] -> Int
calculateTotal func = sum . fmap func

calcWrap :: Size -> Int
calcWrap (Size l w h) = 2 * (l*w+w*h+h*l) + div (l*w*h) (maximum [l,w,h])

calcRibbon :: Size -> Int
calcRibbon (Size l w h) = 2 * (l+w+h) - 2 * maximum [l,w,h] + l*w*h  

day2a :: IO Int
day2a = do
  text <- readFileText "inputs/2.input"
  let input = parseLine <$> T.lines text
  pure $ calculateTotal calcWrap input

day2b :: IO Int
day2b = do
  text <- readFileText "inputs/2.input"
  let input = parseLine <$> T.lines text
  pure $ calculateTotal calcRibbon input

main :: IO ()
main = do
  day2a >>= print
  day2b >>= print
