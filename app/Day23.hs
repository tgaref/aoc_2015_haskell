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
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V 

data Register = A | B deriving stock (Eq, Ord, Show)

data Instruction = Inc {-# UNPACK #-} !Register
                 | Hlf {-# UNPACK #-} !Register
                 | Tpl {-# UNPACK #-} !Register
                 | Jmp {-# UNPACK #-} !Int
                 | Jie {-# UNPACK #-} !Register {-# UNPACK #-} !Int
                 | Jio {-# UNPACK #-} !Register {-# UNPACK #-} !Int
                 deriving stock (Eq, Ord, Show)

-- Read input

readInput :: FilePath -> IO (Vector Instruction)
readInput file = V.fromList . fmap parseLine . T.lines <$> readFileText file

registerP :: Parser Register
registerP = (A.string "a" $> A) <|> (A.string "b" $> B)

offsetP :: Parser Int
offsetP = A.signed A.decimal

instructionP :: Parser Instruction
instructionP = A.choice [ Inc <$> (A.string "inc " *> registerP)
                        , Hlf <$> (A.string "hlf " *> registerP)
                        , Tpl <$> (A.string "tpl " *> registerP)
                        , Jmp <$> (A.string "jmp " *> offsetP)
                        , Jie <$> (A.string "jie " *> registerP) <*> (A.string ", " *> offsetP)
                        , Jio <$> (A.string "jio " *> registerP) <*> (A.string ", " *> offsetP)
                        ]
parseLine :: Text -> Instruction
parseLine t = fromRight (error $ "Failed to parse instrunction: " <> t) (A.parseOnly instructionP t)

-- Solution

data ProgState = PS {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int (Vector Instruction)
  deriving stock (Eq, Show)

eval :: Instruction -> (Int, Int, Int) -> (Int, Int, Int)
eval (Inc A) (a,b,i) = (a+1,b,i+1)
eval (Inc B) (a,b,i) = (a,b+1,i+1)
eval (Hlf A) (a,b,i) = (a `div` 2,b,i+1)
eval (Hlf B) (a,b,i) = (a,b `div` 2,i+1)
eval (Tpl A) (a,b,i) = (3*a,b,i+1)
eval (Tpl B) (a,b,i) = (a,3*b,i+1)
eval (Jmp o) (a,b,i) = (a,b,i+o)
eval (Jie A o) (a,b,i) = if even a then (a,b,i+o) else (a,b,i+1)
eval (Jie B o) (a,b,i) = if even b then (a,b,i+o) else (a,b,i+1)
eval (Jio A o) (a,b,i) = if a == 1 then (a,b,i+o) else (a,b,i+1)
eval (Jio B o) (a,b,i) = if b == 1 then (a,b,i+o) else (a,b,i+1)

run :: State ProgState (Int, Int)
run = do
  (PS a b i vec) <- get
  if i >= V.length vec || i < 0
    then pure (a,b)
    else do let (a',b',i') = eval (vec ! i) (a,b,i)
            put (PS a' b' i' vec)
            run         
              
day23a :: Vector Instruction -> Int
day23a input = snd $ evalState run (PS 0 0 0 input)

day23b :: Vector Instruction -> Int
day23b input = snd $ evalState run (PS 1 0 0 input)

main ::IO ()
main = do
  input <- readInput "inputs/23.input"
  print $ day23a input
  print $ day23b input
