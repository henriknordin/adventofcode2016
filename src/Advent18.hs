--- Day 18: Like a Rogue ---
module Advent18 (advent18) where

import           Lib (getInput)

generate :: String -> [String]
generate seed = seed : generate (next seed)

next :: String -> String
next s = go ('.' : s ++ ".")
  where
    go (a:b:c:xs) = if isTrap a b c then '^' : go (b:c:xs) else '.' : go (b:c:xs)
    go _          = ""

isTrap :: Char -> Char -> Char -> Bool
isTrap a b c
  | a == '^' && b == '^' && c == '.' = True
  | a == '.' && b == '^' && c == '^' = True
  | a == '^' && b == '.' && c == '.' = True
  | a == '.' && b == '.' && c == '^' = True
  | otherwise                        = False

answer :: String -> Int -> Int
answer s rows = length $ filter (== '.') $ concat $ take rows $ generate s

parseInput :: String -> String
parseInput = head . lines

advent18 :: IO ()
advent18 = do
  input <- parseInput <$> getInput 18
  putStrLn $ "Advent 18-1: " ++ show (answer input 40)     -- 2013
  putStrLn $ "Advent 18-2: " ++ show (answer input 400000) -- 20006289

