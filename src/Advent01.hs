-- Day 1: No Time for a Taxicab
--
module Advent01
    ( advent01
    ) where

import Data.List (elemIndex)
import Data.List.Split (splitOn)

import Lib (getInput)

data Direction = North | East | South | West deriving (Eq, Enum, Show)

type Coordinate = (Int, Int)

data Move = 
  Move { turn :: Char
       , step :: Int
       } deriving (Show)

advent01 :: IO ()
advent01 = do
  input <- parseInput <$> getInput 1
  putStrLn $ "Advent 1-1: " ++ show (answer1 input)  -- 300
  putStrLn $ "Advent 1-2: " ++ show (answer2 input)  -- 159

parseInput :: String -> [Move]
parseInput = map parseMove . splitOn ", " . head . lines
  where
    parseMove :: String -> Move
    parseMove s = Move (head s) (read (tail s) :: Int)

answer1 :: [Move] -> Int
answer1 = distance . walk

walk :: [Move] -> Coordinate
walk = go North (0, 0)
  where
    go :: Direction -> Coordinate -> [Move] -> Coordinate
    go _ c [] = c
    go d c (m:ms) = let d' = nextDirection d (turn m)
                        c' = nextCoordinate d' (step m) c
                    in go d' c' ms

answer2 :: [Move] -> Int
answer2 xs = let path = walkWithTrail xs
                 cross = find path
             in case cross of
              [] -> error "No intersection found"
              (c:_) -> distance c
  where
    find :: [Coordinate] -> [Coordinate]
    find []     = []
    find (x:xs) = let i = elemIndex x xs
                  in case i of
                    Nothing -> find xs
                    Just a -> x : find (take a xs)

walkWithTrail :: [Move] -> [Coordinate]
walkWithTrail = go North (0, 0)
  where
    go :: Direction -> Coordinate -> [Move] -> [Coordinate]
    go _ _ [] = []
    go d c (m:ms) = let d' = nextDirection d (turn m)
                        cs = trail d' c (step m) 
                    in cs ++ go d' (last cs) ms

trail:: Direction -> Coordinate -> Int -> [Coordinate]
trail _ _ 0 = []
trail North (x, y) n = (x, y + 1) : trail North (x, y + 1) (n - 1)
trail East  (x, y) n = (x + 1, y) : trail East  (x + 1, y) (n - 1)
trail South (x, y) n = (x, y - 1) : trail South (x, y - 1) (n - 1)
trail West  (x, y) n = (x - 1, y) : trail West  (x - 1, y) (n - 1)

nextDirection :: Direction -> Char -> Direction
nextDirection d 'L' = if d == North then West else pred d
nextDirection d 'R' = if d == West then North else succ d

nextCoordinate :: Direction -> Int -> Coordinate -> Coordinate 
nextCoordinate North i (x, y) = (x, y + i)
nextCoordinate East  i (x, y) = (x + i, y)
nextCoordinate South i (x, y) = (x, y - i)
nextCoordinate West  i (x, y) = (x - i, y)

distance :: Coordinate -> Int
distance (x, y) = abs x + abs y

