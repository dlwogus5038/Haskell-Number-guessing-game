module Guess (
  GameState, 
  initialize, 
  guess,
  refine,
  checkEqualPQ,
  checkEqualP,
  checkEqualQ,
  makeString,
  checkValid
) where

import System.Random
import Data.List

type GameState = [Int]

initialize :: (Int -> Bool) -> GameState
initialize _ = filter checkValid [1..9999]

checkValid :: Int -> Bool
checkValid n = if ((n `div` 1000) /= 0)
               then 100 <= n && n <= 9999 && (length (Data.List.nub (show n)) == 4)
               else 100 <= n && n <= 9999 && (length (Data.List.nub ("0" ++ show n)) == 4)

guess :: RandomGen g => g -> GameState -> (Int, GameState)
guess g s = ((s!!) $ fst $ randomR (0000,  length s - 1) g, s)

refine :: (Int, GameState) -> (Int, Int) -> GameState
refine (x, s) (p, q) = filter (checkEqualPQ x p q) s

checkEqualPQ :: Int -> Int -> Int -> Int -> Bool
checkEqualPQ x p q sElem = ((checkEqualP (makeString x) (makeString sElem)) == p) && ((checkEqualQ (makeString x) (makeString sElem)) == q)

makeString :: Int -> String
makeString n = let str = (show n)
               in if (length str == 3)
                  then "0" ++ str
                  else str

checkEqualP :: String -> String -> Int
checkEqualP xs sElem = if (length xs == 0) then 0
                       else if ((head xs) `elem` sElem) then 1 + checkEqualP (tail xs) sElem
                       else checkEqualP (tail xs) sElem

checkEqualQ :: String -> String -> Int
checkEqualQ xs sElem = let a = if ((xs !! 0) == (sElem !! 0)) then 1 else 0
                           b = if ((xs !! 1) == (sElem !! 1)) then 1 else 0
                           c = if ((xs !! 2) == (sElem !! 2)) then 1 else 0
                           d = if ((xs !! 3) == (sElem !! 3)) then 1 else 0
                       in (a+b+c+d)

