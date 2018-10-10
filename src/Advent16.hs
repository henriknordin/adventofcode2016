--- Day 16: Dragon Checksum ---
module Advent16 (advent16) where

import           Data.Char (digitToInt, intToDigit)

import           Lib (getInput)

answer :: [Int] -> Int -> String
answer xs i = (map intToDigit . checksum . take i) $ xs ++ generate xs

checksum :: [Int] -> [Int]
checksum xs
  | (even . length) xs = (checksum . reduce) xs
  | otherwise          = xs
  where
    reduce :: [Int] -> [Int]
    reduce (x:y:xs) = if x == y then 1:reduce xs else 0:reduce xs
    reduce []       = []

generate :: [Int] -> [Int]
generate xs = extended ++ generate block
  where
    extended = extend xs
    block = xs ++ extended

extend :: [Int] -> [Int]
extend xs = 0 : (map (\x -> if x == 1 then 0 else 1) . reverse) xs

parseInput :: String -> [Int]
parseInput = map digitToInt . head . lines

advent16 :: IO ()
advent16 = do
  input <- parseInput <$> getInput 16
  putStrLn $ "Advent 16-1: " ++ show (answer input 272)      -- 10010110010011110
  putStrLn $ "Advent 16-2: " ++ show (answer input 35651584) -- 01101011101100011

