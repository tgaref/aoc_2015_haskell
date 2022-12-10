{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

Advent of Code 2015 using Haskell
-}

module Main
       ( main
       ) where

import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as M 
import           Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ

type Mana = Int

data Spell = MagicMissile | Drain | Shield | Poison | Recharge 
           deriving stock (Eq, Ord, Show)

data GameState = GS { playerHP   :: {-# UNPACK #-} !Int
                    , playerMana :: {-# UNPACK #-} !Int
                    , bossHP     :: {-# UNPACK #-} !Int
                    , timer      :: Map Spell Int
                    } deriving stock (Eq, Ord, Show)

cost :: Spell -> Int
cost MagicMissile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

damage :: Spell -> Int
damage MagicMissile = 4
damage Drain = 2    
damage Poison = 3
damage _ = 0

duration :: Spell -> Int
duration MagicMissile = 1
duration Drain = 1     
duration Shield = 6
duration Poison = 6
duration Recharge = 5

indicator :: Int -> Int
indicator t = if t /= 0 then 1 else 0

player :: Int -> GameState -> Spell -> GameState
player difficulty (GS playerHP playerMana bossHP timer) spell
  | playerHP <= difficulty = GS 0 0 1 M.empty
  | otherwise              = GS playerHP' playerMana' bossHP' timer''
  where
    timer' = M.insert spell (duration spell) timer
    timer'' = M.map (\t -> max 0 (t-1)) timer'
    dmg = M.foldlWithKey' (\acc sp t -> acc + (indicator t) * (damage sp)) 0 timer'
    playerHP' = playerHP - difficulty + if timer' ! Drain > 0 then 2 else 0
    playerMana' = playerMana - cost spell + if timer' ! Recharge > 0 then 101 else 0
    bossHP' = bossHP - dmg

boss :: Int -> GameState -> GameState
boss dmg (GS playerHP playerMana bossHP timer) = GS playerHP' playerMana' bossHP' timer'
  where
    timer' = M.map (\t -> max 0 (t-1)) timer
    bossHP' = bossHP - if timer ! Poison > 0 then 3 else 0
    playerHP' = playerHP - dmg + if timer ! Shield > 0 then 7 else 0 + if timer ! Drain > 0 then 2 else 0 
    playerMana' = playerMana + if timer ! Recharge > 0 then 101 else 0

playerTurn :: Int -> GameState -> Mana -> [(GameState, Mana)]
playerTurn difficulty gs mana = gss
  where
    tmr = timer gs
    gss = (\spell -> (player difficulty gs spell, mana + cost spell))
          <$> filter (\spell -> tmr ! spell == 0 && cost spell <= playerMana gs) [MagicMissile, Drain, Shield, Poison, Recharge]

shortestPath :: Int -> Int -> (GameState -> Bool) -> GameState -> Mana
shortestPath difficulty dmg finished start = go (PQ.singleton start 0)
  where
    go :: PSQ GameState Mana -> Mana
    go togo
      | finished gameState = mana
      | otherwise          = go togo'' 
      where
        (st,togo') = fromMaybe (error "Impossible!") $ PQ.minView togo
        gameState = PQ.key st
        mana = PQ.prio st
        neigh = playerTurn difficulty gameState mana
        wins = filter (\(gs,_) -> bossHP gs <= 0) neigh
        nonLoses = filter (\(gs,_) -> playerHP gs > 0) neigh
        neigh' = fmap (\(gs,cst) -> (boss dmg gs,cst)) nonLoses
        nonLoses' = filter (\(gs,_) -> playerHP gs > 0) neigh'
        togo'' = foldl' (\acc (s,cst) -> PQ.insertWith min s cst acc) togo' (wins <> nonLoses')

day22a :: Int
day22a = shortestPath 0 9 (\gs -> bossHP gs <= 0) (GS 50 500 51 timers)
  where
    timers = M.fromList [(MagicMissile,0), (Drain,0), (Shield,0), (Poison,0), (Recharge,0)]

day22b :: Int
day22b = shortestPath 1 9 (\gs -> bossHP gs <= 0) (GS 50 500 51 timers)
  where
    timers = M.fromList [(MagicMissile,0), (Drain,0), (Shield,0), (Poison,0), (Recharge,0)]

main ::IO ()
main = do
  print day22a
  print day22b
