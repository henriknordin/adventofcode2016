--- Day 19: An Elephant Named Joseph ---
module Advent19 (advent19) where

import           Data.Sequence
import           Prelude hiding (length)

import           Lib (getInput)

answer1 :: Seq Int -> Int
answer1 (x :<| Empty)    = x
answer1 (x :<| y :<| xs) = answer1 $ xs |> x

answer2 :: Seq Int -> Int
answer2 (x :<| Empty) = x
answer2 xs            = let (x :<| xs') = steal xs
                        in answer2 $ xs' |> x

steal :: Seq Int -> Seq Int
steal xs = deleteAt (n `div` 2) xs
  where
    n = length xs
    
parseInput :: String -> Int
parseInput = read . head . lines

advent19 :: IO ()
advent19 = do
  input <- parseInput <$> getInput 19
  putStrLn $ "Advent 19-1: " ++ show (answer1 $ fromList [0..input]) -- 1842613
  putStrLn $ "Advent 19-2: " ++ show (answer2 $ fromList [0..input]) -- 1424135

