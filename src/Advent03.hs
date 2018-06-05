-- Day 3: Squares With Three Sides
--
module Advent03
    ( parseInput
    , answer1
    , answer2
    ) where

import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)

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

