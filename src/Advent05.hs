-- Day 5: How About a Nice Game of Chess?
--
module Advent05
    ( answer1
    , answer2
    , validHashes
    , advent05
    ) where

import           Data.Char (digitToInt)
import           Data.List (sortBy, nubBy)
import           Data.Ord (comparing)

import           Advent.Hash (toMD5)
import           Advent.Lib (getInput)


hashes :: String -> [String]
hashes seed = toMD5 . (seed ++) . show <$> [(0 :: Int)..]

valid :: String -> Bool
valid s = "00000" == take 5 s

validHashes :: String -> [String]
validHashes = filter valid . hashes

parseInput :: String -> String
parseInput = head . lines

answer1 :: [String] -> String
answer1 = take 8 . map (!! 5)

answer2 :: [String] -> String
answer2 = map snd 
        . sortBy (comparing fst) 
        . take 8 
        . nubBy position 
        . filter (\c -> fst c < 8) 
        . map toPair
  where
    toPair :: String -> (Int, Char)
    toPair s = (digitToInt . (!! 5) $ s, s !! 6)
    position a b = fst a == fst b

advent05 :: IO ()
advent05 = do
  input <- parseInput <$> getInput 5
  let possible = validHashes input
  putStrLn $ "Advent 5-1: " ++ show (answer1 possible)  -- f97c354d
  putStrLn $ "Advent 5-2: " ++ show (answer2 possible)  -- 863dde27
