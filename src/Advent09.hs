-- Day 9: Explosives in Cyberspace
--
module Advent09
    ( advent09
    ) where

import           Data.List.Split (splitOn)
import           Lib (getInput)

advent09 :: IO ()
advent09 = do
  input <- parseInput <$> getInput 9
  putStrLn $ "Advent 9-1: " ++ show (answer1 input)  -- 115118
  putStrLn $ "Advent 9-2: " ++ show (answer2 input)  -- 11107527530

parseInput :: String -> String
parseInput = head . lines

answer1 :: String -> Int
answer1 = length . decompress

answer2 :: String -> Int 
answer2 = decompressLength

decompress :: String -> String
decompress  []      = []
decompress ('(':xs) = (concat . replicate rep) (take len rest) ++ decompress (drop len rest)
                        where
                          (rest, Rule len rep) = parseRule xs
decompress (x:xs)   = x : decompress xs

decompressLength :: String -> Int
decompressLength [] = 0
decompressLength ('(':xs) = rep * decompressLength (take len rest) + decompressLength (drop len rest)
                              where
                                (rest, Rule len rep) = parseRule xs
decompressLength (x:xs)   = 1 + decompressLength xs

parseRule :: String -> (String, Rule)
parseRule s = (rest, Rule (read len) (read reps))
  where
    (rule, _:rest) = span (/= ')') s
    [len, reps] = splitOn "x" rule
    sequ = take (read len) rest

data Rule = Rule !Int !Int deriving (Show)

