{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

-- import           Lib (simulate)
import qualified Data.Text as T
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A 

type Element = Text
data Rule = Rule Element [Element]
  deriving stock (Eq, Ord, Show)

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

medicine :: [Element]
medicine = case A.parseOnly medP medicine' of
  Left _  -> error "Failed to parse medicine..."
  Right a -> a

medicine' :: Text
--medicine' = "HOH"
medicine' = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"               

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

splitList :: Eq a => [a] -> a -> [[a]]
splitList []   _ = []
splitList list a = seg : splitList rest a
  where
    (seg, rest) = go [] list 
    go ys []     = (ys, [])
    go ys (x:xs)
      | x == a = (reverse (x:ys), xs)
      | otherwise = go (x:ys) xs
    
nonTerminals :: Set [Element]
nonTerminals = S.fromList [["Al"], ["B"], ["Ca"], ["F"], ["H"], ["Mg"], ["N"], ["O"], ["P"], ["Si"], ["Th"], ["Ti"]]

day19a :: Map Element [[Element]] -> Int
day19a m = S.size $ oneReplacement m medicine

test = seg
  where
    seg = case segments of
      []     -> error "No input..."
      (a:_) -> a
    target = medicine
--    target = ["H","O","H","O","H","O"] :: [Element]
--    bound = length target
    segments = splitList target "Ar"

day19b :: Map Element [[Element]] -> Int
day19b m = go (S.fromList [["e"]]) 0
  where
    target = medicine
--    target = ["H","O","H","O","H","O"] :: [Element]
    bound = length target
    go molecules !k
      | target `S.member` molecules = k
      | otherwise =
        let molecules' = S.filter (\mol -> length mol <= bound) $ S.foldl' S.union S.empty $ S.map (oneReplacement m) molecules
        in go molecules' (k+1)


main ::IO ()
main = do 
  input <- readInput "inputs/19.input"
  let rules = M.fromListWith (<>) $ fmap (\(Rule e es) -> (e, [es])) input
--  print rules
  --  print $ day19a rules
  print $ day19b rules
