-- Day 3: Squares With Three Sides
--
module Advent03
    ( advent03
    ) where

import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)

import Lib (getInput)

advent03 :: IO ()
advent03 = do
  input <- parseInput <$> getInput 3
  putStrLn $ "Advent 3-1: " ++ show (answer1 input)  -- 1050
  putStrLn $ "Advent 3-2: " ++ show (answer2 input)  -- 1921

parseInput :: String -> [[Int]]
parseInput = map (map (\s -> read s :: Int) . words) . lines

answer1 :: [[Int]] -> Int
answer1 = length . filter possibleTriangle

answer2 :: [[Int]] -> Int
answer2 = length . filter possibleTriangle . rotate

rotate :: [[Int]] -> [[Int]]
rotate = chunksOf 3 . concat . transpose

-- https://en.wikipedia.org/wiki/Triangle_inequality
possibleTriangle :: [Int] -> Bool
possibleTriangle t = 2 * maximum t < sum t

