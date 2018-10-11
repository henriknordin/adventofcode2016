-- Day 6: Signals and Noise
--
module Advent06
    ( advent06
    ) where

import           Data.List (group, sort, minimumBy, maximumBy, transpose)
import           Data.Ord (comparing)

import           Advent.Lib (getInput)


advent06 :: IO ()
advent06 = do
  input <- parseInput <$> getInput 6
  putStrLn $ "Advent 6-1: " ++ show (answer1 input)  -- mlncjgdg
  putStrLn $ "Advent 6-2: " ++ show (answer2 input)  -- bipjaytb

parseInput :: String -> [String]
parseInput = lines

answer1 :: [String] -> String
answer1 = map mostFrequent . transpose

mostFrequent :: String -> Char
mostFrequent = head . maximumBy (comparing length) . group . sort 

answer2 :: [String] -> String
answer2  = map leastFrequent . transpose

leastFrequent :: String -> Char
leastFrequent = head . minimumBy (comparing length) . group . sort 

