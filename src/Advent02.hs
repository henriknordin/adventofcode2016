-- Day 2: Bathroom Security
--
module Advent02
    ( advent02
    ) where

import Data.Char (intToDigit)

import Lib (getInput)

type Coordinate = (Int, Int)

advent02 :: IO ()
advent02 = do
  input <- parseInput <$> getInput 2
  putStrLn $ "Advent 2-1: " ++ show (answer1 input)  -- 24862
  putStrLn $ "Advent 2-2: " ++ show (answer2 input)  -- 46C91

parseInput :: String -> [String]
parseInput = lines

answer1 :: [String] -> String
answer1 = go (1, 1)
  where
    go :: Coordinate -> [String] -> String
    go _ [] = []
    go c (x:xs) = let c' = move1 c x
                      x' = fst c'
                      y' = snd c'
                  in keypad1 !! y' !! x' : go c' xs

answer2 :: [String] -> String
answer2 = go (0, 2)
  where
    go :: Coordinate -> [String] -> String
    go _ [] = []
    go d (x:xs) = let (x', y') = move2 d x
                  in keypad2 !! y' !! x' : go (x', y') xs

move1 :: Coordinate -> String -> Coordinate
move1 = foldl nextDigit
  where
    nextDigit :: Coordinate -> Char -> Coordinate
    nextDigit (x, y) 'U' = if y > 0 then (x, y - 1) else (x, y)
    nextDigit (x, y) 'D' = if y < 2 then (x, y + 1) else (x, y)
    nextDigit (x, y) 'L' = if x > 0 then (x - 1, y) else (x, y)
    nextDigit (x, y) 'R' = if x < 2 then (x + 1, y) else (x, y)


move2 :: Coordinate -> String -> Coordinate
move2 = foldl nextDigit
  where
    nextDigit :: Coordinate -> Char -> Coordinate
    nextDigit (x, y) 'U' = if y > 0 && keypad2 !! (y-1) !! x /= ' ' then (x, y - 1) else (x, y)
    nextDigit (x, y) 'D' = if y < 4 && keypad2 !! (y+1) !! x /= ' ' then (x, y + 1) else (x, y)
    nextDigit (x, y) 'L' = if x > 0 && keypad2 !! y !! (x-1) /= ' ' then (x - 1, y) else (x, y)
    nextDigit (x, y) 'R' = if x < 4 && keypad2 !! y !! (x+1) /= ' ' then (x + 1, y) else (x, y)

keypad1 :: [String]
keypad1 =
  [ "123"
  , "456"
  , "789" 
  ]

keypad2 :: [String]
keypad2 =
  [ "  1  "
  , " 234 "
  , "56789"
  , " ABC "
  , "  D  "
  ]

test' :: String
test' = "ULL\n\
        \RRDDD\n\
        \LURDL\n\
        \UUUUD"

