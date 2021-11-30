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
import qualified Data.Map.Strict as M
import Data.Bits


data Operand = Wire Text | Signal Word16
  deriving stock (Eq, Ord, Show)

data Command = NOT !Operand
             | AND !Operand !Operand
             | OR !Operand !Operand
             | LSHIFT !Operand !Operand
             | RSHIFT !Operand !Operand
             | NONE !Operand
  deriving stock (Eq, Ord, Show)

readInput :: FilePath -> IO [Text]
readInput file = T.lines <$> readFileText file

wireP :: Parser Operand
wireP = fmap (Wire . T.pack) (A.many1 A.letter)

signalP :: Parser Operand
signalP = fmap Signal A.decimal

operandP :: Parser Operand
operandP = signalP <|> wireP

rightSideP :: Parser Operand
rightSideP = A.string " -> " *> wireP

notP :: Parser (Command, Operand)
notP = (,) <$> (NOT <$> (A.string "NOT " *> operandP)) <*> rightSideP

andP :: Parser (Command, Operand)
andP = (,) <$> (AND <$> operandP <*> (A.string " AND " *> operandP)) <*> rightSideP 

orP :: Parser (Command, Operand)
orP = (,) <$> (OR <$> operandP <*> (A.string " OR " *> operandP)) <*> rightSideP 

lshiftP :: Parser (Command, Operand)
lshiftP = (,) <$> (LSHIFT <$> operandP <*> (A.string " LSHIFT " *> operandP)) <*> rightSideP 

rshiftP :: Parser (Command, Operand)
rshiftP = (,) <$> (RSHIFT <$> operandP <*> (A.string " RSHIFT " *> operandP)) <*> rightSideP 

noneP :: Parser (Command, Operand)
noneP = (,) <$> (NONE <$> operandP) <*> rightSideP

commandP :: Parser (Command, Operand)
commandP = A.choice [notP, andP, orP, lshiftP, rshiftP, noneP]

parseLine :: Text -> (Operand, Command)
parseLine t = case A.parseOnly commandP t of
  Right (c,o) -> (o,c)
  Left _  -> error "Failed to parse command..."

type Circuit = ReaderT (Map Operand Command) (State (Map Operand Word16)) Word16

evalC :: Command -> Map Operand Command -> State (Map Operand Word16) Word16
evalC (NOT a)      cs = do
  v <- compute a cs
  modify' $ M.insert a v
  pure $ complement v
  
evalC (AND a b)    cs = do
  va <- compute a cs
  modify' $ M.insert a va
  vb <- compute b cs
  modify' $ M.insert b vb
  pure $ va .&. vb
  
evalC (OR a b)     cs = do
  va <- compute a cs
  modify' $ M.insert a va
  vb <- compute b cs
  modify' $ M.insert b vb
  pure $ va .|. vb

evalC (LSHIFT a b) cs = do
  va <- compute a cs
  modify' $ M.insert a va
  vb <- compute b cs
  modify' $ M.insert b vb
  pure $ va `shiftL` fromIntegral vb

evalC (RSHIFT a b) cs = do
  va <- compute a cs
  modify' $ M.insert a va
  vb <-  compute b cs
  modify' $ M.insert b vb
  pure $ va `shiftR` fromIntegral vb

evalC (NONE a)     cs = compute a cs

compute :: Operand -> Map Operand Command -> State (Map Operand Word16) Word16
compute (Signal a)   _  = pure a
compute w@(Wire t)   cs = do
  m <- get
  case M.lookup w m of
    Just a  -> pure a 
    Nothing -> evalC c cs
  where
    c = case M.lookup (Wire t) cs of
          (Just a) -> a
          Nothing      -> error "Failed to find wire..." 
                          
day7a :: IO Word16
day7a = do
  input <- readInput "inputs/7a.input"
  let commands = reverse $ fmap parseLine input  
  pure $ evalState (compute (Wire "a") (M.fromList commands)) M.empty 
  

day7b :: IO Word16
day7b = do
  input <- readInput "inputs/7b.input"
  let commands = reverse $ fmap parseLine input  
  pure $ evalState (compute (Wire "a") (M.fromList commands)) M.empty 

main ::IO ()
main = do
  day7a >>= print
  day7b >>= print
