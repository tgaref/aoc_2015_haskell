{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

import Crypto.Hash
import Data.ByteArray.Encoding (convertToBase, Base (Base16))

import qualified Data.ByteString as B

solve :: ByteString -> ByteString -> Int
solve secret start = go 0
  where
    n = B.length start
    go !k =
      let kHex = show k :: ByteString
          digest :: Digest MD5
          digest = hash $ secret <> kHex
          result = convertToBase Base16 digest :: ByteString
      in
        if B.take n result == start
        then k
        else go (k+1)
  
day4a :: Int
day4a = solve "bgvyzdsv" "00000" 
  
day4b :: Int
day4b = solve "bgvyzdsv" "000000" 

main ::IO ()
main = do
  print day4a
  print day4b
