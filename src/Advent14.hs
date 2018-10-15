--- Day 14: One-Time Pad ---
module Advent14 (advent14) where

import           Data.List (find, isInfixOf)
import           Data.Maybe (isJust)

import           Advent.Hash (toMD5)
import           Advent.Lib (getInput)


answer :: [String]-> Int
answer s = go 0 s !! 63
  where
    go :: Int -> [String] -> [Int]
    go index (h:hs) = 
      if isKey h hs
        then index:go (index + 1) hs
        else go (index + 1) hs

isKey :: String -> [String] -> Bool
isKey s xs = 
  case possKey s of
    Just c  -> isJust $ find (checkKey c) $ take 1000 xs
    Nothing -> False

possKey :: String -> Maybe Char
possKey (a:b:c:xs) = if a == b && a == c then Just a else possKey (b:c:xs)
possKey _          = Nothing

checkKey :: Char -> String -> Bool
checkKey c s = replicate 5 c `isInfixOf` s

hashes :: String -> [String]
hashes salt = map (\a -> toMD5 $ salt ++ show a)  [0..]

strechedHashes :: String -> [String]
strechedHashes salt = map (\a -> iterate toMD5 (salt ++ show a) !! 2017)  [0..]

parseInput :: String -> String
parseInput = head . words . head . lines

advent14 :: IO ()
advent14 = do
  input <- parseInput <$> getInput 14
  putStrLn $ "Advent 14-1: " ++ show (answer $ hashes input) -- 35186
  putStrLn $ "Advent 14-1: " ++ show (answer $ strechedHashes input) -- 22429

