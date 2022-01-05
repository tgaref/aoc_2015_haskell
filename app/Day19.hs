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
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A 

type Element = Text
data Rule = Rule Element [Element]
  deriving stock (Eq, Ord, Show)

-- Read input

readInput :: FilePath -> IO [Rule]
readInput file = fmap parseRule . T.lines <$> readFileText file

elements :: [Element]
elements = [ T.singleton a <> T.singleton b | a <- ['A' .. 'Z'], b <- ['a' .. 'z']] <> fmap T.singleton ['A' .. 'Z'] <> ["e"]

elementP :: Parser Element
elementP = A.choice $ A.string <$> elements

ruleP :: Parser Rule
ruleP = Rule <$> (elementP <* A.string " => ") <*> A.many1 elementP

parseRule :: Text -> Rule
parseRule t = case A.parseOnly ruleP t of
  Left _  -> error $ "Failed to parse rule... " <> t
  Right a -> a

medP :: Parser [Element]
medP = A.many1 elementP

-- Solution

medicine :: Text
medicine = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"               

toElements :: Text -> [Element]
toElements t = case A.parseOnly medP t of
  Left _  -> error "Failed to parse medicine..."
  Right a -> a 

simplify :: Text -> Char
simplify "Rn" = '('
simplify "Ar" = ')'
simplify "Y" = ','
simplify _ = 'x'

replaceAt :: Int -> [a] -> [a] -> [a]
replaceAt n input r = go [] input 1
  where
    go _  []     _  = error "Out of range..."
    go xs (y:ys) !i
      | i >= n = reverse xs <> (r <> ys)
      | i < n  = go (y:xs) ys (i+1)

oneReplacement :: Map Element [[Element]] -> [Element] -> Set [Element] 
oneReplacement m molecule = foldl' (\acc (i,c) ->
                                      case m !? c of
                                        Nothing -> acc
                                        Just rs -> acc `S.union` S.fromList (replaceAt i molecule <$> rs)
                                   ) S.empty $ zip [1..] molecule

splitTill :: (a -> Bool) -> [a] -> ([a], [a])
splitTill f = go []
  where
    go acc [] = (acc, [])
    go acc (x:xs)
      | f x    = (x : acc, xs)
      | otherwise = go (x : acc) xs

splitTill1 :: (a -> Bool) -> [a] -> ([a], [a])
splitTill1 f = go []
  where
    go acc []  = (acc, [])
    go _   [_] = error "Should not happen..."
    go acc (x:y:xs)
      | f x    = (y : x : acc, xs)
      | otherwise = go (x : acc) (y:xs)

takeSeg :: [Char] -> ([Char], [Char], [Char])
takeSeg elts = (reverse rest', seg, rest)
  where
    (start, rest) = splitTill (== ')') elts
    (seg, rest') = splitTill1 (== '(') start
                            
day19a :: Map Element [[Element]] -> Int
day19a m = S.size $ oneReplacement m (toElements medicine)

day19b :: Int
day19b = go (simplify <$> toElements medicine) 0
  where
    count seg = length seg - 2 * commas - 3
      where
        commas = length (filter (== ',') seg)
          
    go molecule !acc
      | '(' `notElem` molecule = acc + length molecule - 1
      | otherwise = go (a <> ['x'] <> c) (acc + k) 
        where
          (a,b,c) = takeSeg molecule
          k = count b 

main ::IO ()
main = do 
  input <- readInput "inputs/19.input"
  let rules = M.fromListWith (<>) $ fmap (\(Rule e es) -> (e, [es])) input
  print $ day19a rules
  print day19b

