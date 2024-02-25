{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where


data Weapon = W {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

data Armor = A {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

data Ring = R {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

data Character = C { hitpoints :: {-# UNPACK #-} !Int
                   , damage    :: {-# UNPACK #-} !Int
                   , armor     :: {-# UNPACK #-} !Int
                   } deriving stock (Eq, Ord, Show)

shopWeapons :: [Weapon]
shopWeapons = [W 8 4 0, W 10 5 0, W 25 6 0, W 40 7 0, W 74 8 0]

shopArmor :: [Armor]
shopArmor = [A 13 0 1, A 31 0 2, A 53 0 3, A 75 0 4, A 102 0 5]

shopRings :: [Ring]
shopRings = [R 25 1 0, R 50 2 0, R 100 3 0, R 20 0 1, R 40 0 2, R 80 0 3]

boss :: Character
boss = C 104 8 1

wins :: Character -> Character -> Bool
wins (C h1 d1 a1) (C h2 d2 a2) = n1 <= n2
  where
    blow1 = max (d1-a2) 1
    blow2 = max (d2-a1) 1
    n1 = case quotRem h2 blow1 of
           (a, 0) -> a
           (a, _) -> a+1
    n2 = case quotRem h1 blow2 of
           (a, 0) -> a
           (a, _) -> a+1

characteristics :: Int -> Weapon -> Armor -> Ring -> Ring -> (Character, Int)
characteristics hitpoints (W a1 b1 _) (A a2 _ c2) (R a3 b3 c3) (R a4 b4 c4) = (C hitpoints damage armor, cost)
  where
    damage = b1 + b3 + b4
    armor = c2 + c3 + c4
    cost = a1 + a2 + a3 + a4

tryall :: Int -> [Weapon] -> [Armor] -> [Ring] -> [(Character, Int)]
tryall h ws as rs = do
  w <- ws
  a <- (A 0 0 0) : as
  r1 <- (R 0 0 0) : rs
  r2 <- (R 0 0 0) : rs
  guard $ r1 /= r2
  pure $ characteristics h w a r1 r2 
  
day21a :: Int
day21a = foldl' (\acc (me, cost) -> if me `wins` boss && cost < acc then cost else acc) maxBound possibilities
  where
    possibilities = tryall 100 shopWeapons shopArmor shopRings

day21b :: Int
day21b = foldl' (\acc (me, cost) -> if not (me `wins` boss) && cost > acc then cost else acc) 0 possibilities
  where
    possibilities = tryall 100 shopWeapons shopArmor shopRings

main ::IO ()
main = do
  print $ day21a
  print $ day21b
