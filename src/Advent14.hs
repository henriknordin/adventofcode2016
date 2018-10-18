--- Day 14: One-Time Pad ---
module Advent14 (advent14) where

import           Data.ByteString.Char8 as C (unpack)
import           Data.List (isInfixOf)
import           Data.Maybe (isJust, fromJust)
import           Data.String (fromString)

import           Advent.Hash (toMd5Raw)
import           Advent.Lib (getInput, applyN)

answer :: [String] -> Int
answer hs = (!! 63) $ fiveConsec
           $ map (\(a, b, c) -> (a, b, fromJust c))
           $ filter (\(a, b, c) -> isJust c)
           $ map (\(a, b) -> (a, b, possKey b))
           $ zip [0..] hs

fiveConsec :: [(Int, String, Char)] -> [Int]
fiveConsec ((i, s, c):xs) = if helper xs
                              then i:fiveConsec xs
                              else fiveConsec xs
 where
   helper ((i', s', _):xs)
     | i' - i <= 1000 && checkKey c s' = True
     | i' - i >  1000                  = False
     | otherwise                       = helper xs

possKey :: String -> Maybe Char
possKey (a:b:c:xs) = if a == b && a == c then Just a else possKey (b:c:xs)
possKey _          = Nothing

checkKey :: Char -> String -> Bool
checkKey c s = replicate 5 c `isInfixOf` s

hashes :: Int -> String -> [String]
hashes n salt = map (\a -> C.unpack (applyN n toMd5Raw (fromString $ salt ++ show a)))  [0..]

parseInput :: String -> String
parseInput = head . words . head . lines

advent14 :: IO ()
advent14 = do
  input <- parseInput <$> getInput 14
  putStrLn $ "Advent 14-1: " ++ show (answer $ hashes    1 input) -- 35186
  putStrLn $ "Advent 14-2: " ++ show (answer $ hashes 2017 input) -- 22429
