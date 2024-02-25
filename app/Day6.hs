{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

-- import Lib
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as M 
import Control.Foldl (Fold)
import qualified Control.Foldl as F

data Position = P {-# UNPACK#-} !Int {-# UNPACK#-} !Int
  deriving stock (Eq, Ord, Show)
data Action = On | Off | Toggle
  deriving stock (Eq, Ord, Show)
data Instruction = I Action Position Position
  deriving stock (Eq, Ord, Show)

readInput :: IO [Text]
readInput = T.lines <$> readFileText "inputs/6.input"

actionP :: Parser Action
actionP = On <$ A.string "turn on" <|> Off <$ A.string "turn off" <|> Toggle <$ A.string "toggle"

posP :: Parser Position
posP = P <$> A.decimal <*> (A.char ',' *> A.decimal)

instrP :: Parser Instruction
instrP = I <$> actionP <*> (A.skipSpace *> posP) <*> (A.string " through " *> posP)

parseInstruction :: Text -> Instruction
parseInstruction t = case A.parseOnly instrP t of
  Right instr -> instr
  Left  _     -> error "Error parsing instruction!"
  
day6a :: IO Int
day6a = do
  input <- readInput
  let gridSize = 1000
  let instructions = fmap parseInstruction input
  let grid = M.fromList [(P x y, 0) | x <- [0 .. gridSize-1], y <- [0 .. gridSize-1]]
  let finalGrid = foldl' takeAction grid instructions
  pure $ F.fold F.sum finalGrid
  where
    takeAction :: Map Position Int -> Instruction -> Map Position Int 
    takeAction grid (I action (P x1 y1) (P x2 y2)) =
      case action of 
        On     -> F.fold (adjustF grid (const 1)) [P x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
        Off    -> F.fold (adjustF grid (const 0)) [P x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
        Toggle -> F.fold (adjustF grid (1 - )) [P x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

adjustF :: Map Position Int -> (Int -> Int) -> Fold Position (Map Position Int)
adjustF grid f = F.Fold step grid identity
  where
    step g p = M.adjust f p g  

day6b :: IO Int
day6b = do
  input <- readInput
  let gridSize = 1000
  let instructions = fmap parseInstruction input
  let grid = M.fromList [(P x y, 0) | x <- [0 .. gridSize-1], y <- [0 .. gridSize-1]]
  let finalGrid = foldl' takeAction grid instructions
  pure $ F.fold F.sum finalGrid
  where
    takeAction :: Map Position Int -> Instruction -> Map Position Int 
    takeAction grid (I action (P x1 y1) (P x2 y2)) =
      case action of 
        On     -> F.fold (adjustF grid (+ 1)) [P x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
        Off    -> F.fold (adjustF grid (\n -> max 0 (n-1))) [P x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
        Toggle -> F.fold (adjustF grid (+ 2)) [P x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

main ::IO ()
main = do
  day6a >>= print
  day6b >>= print
