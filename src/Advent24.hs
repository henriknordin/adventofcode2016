--- Day 24: Air Duct Spelunking ---
module Advent24 where

import           Data.Array
import           Data.Char (isDigit)
import           Data.List (sortOn, permutations)
import           Data.Maybe (fromJust, isJust)

import           Advent.Search (aStar)
import           Advent.Lib (getInput)


type Grid = Array (Int, Int) Char

type Coordinate = (Int, Int)

type Vertex = Coordinate
type Edge = (Vertex, Vertex, Int)


parseInput :: String -> Grid
parseInput input = array ((0 ,0), (sizeX - 1, sizeY - 1)) 
                     [((x, y), (rows !! y) !! x) | x <- [0 .. sizeX-1], y <- [0 .. sizeY-1]]
  where
    rows = lines input
    sizeY = length rows
    sizeX = length (head rows)

heuristic :: Coordinate -> Coordinate -> Int
heuristic (v1x, v1y) (v2x, v2y) = abs (v1x - v2x) + abs (v1y - v2y)

generateEdges :: Grid -> [Vertex] -> [Edge]
generateEdges g vs = map (\(v1, v2, mw) -> (v1, v2, fst $ fromJust mw)) 
                   $ filter (\(c1, c2, w) -> isJust w) 
                   $ map (\(c1, c2) -> (c1, c2, aStar c1 (`heuristic` c2) (generator g) (== c2) id)) pairs
  where
    pairs = [(v1, v2) | v1 <- vs, v2 <- vs, v1 /= v2]

generator :: Grid -> Coordinate -> [Coordinate]
generator g (x,y) = filter (\(x, y) -> g ! (x, y) /= '#') 
                  $ filter isBounded 
                  $ map (\(ox, oy) -> (x + ox, y + oy)) offsets
  where
    offsets :: [(Int, Int)]
    offsets = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    isBounded :: (Int, Int) -> Bool -- TODO Change to Coordinate
    isBounded (x, y) = x >= x0 && x <= x1 && y >= y0 && y <= y1
      where
        ((x0,y0),(x1,y1)) = bounds g

-- total number of premutations is the length of numbers minus the starting point
distance :: [(Int, Int)] -> [Edge] -> Int
distance xs edges = go xs 
  where
    go :: [(Int, Int)] -> Int
    go []       = 0
    go [x]      = 0
    go (a:b:xs) = let dist = findEdgeDistance edges a b
                  in dist + go (b:xs)

findEdgeDistance :: [Edge] -> (Int, Int) -> (Int, Int) -> Int
findEdgeDistance es v1 v2 = head $ map (\(_, _, s) -> s) $ filter (\(v1', v2', _) -> v1 == v1' && v2 == v2') es

answer1 :: Grid -> Int
answer1 g = minimum $ map (`distance` edges) paths
  where
    numbers = sortOn snd $ filter (\(_, x) -> isDigit x) $ assocs g
    vertices = map fst numbers
    edges = generateEdges g vertices
    startVertex = fst $ head numbers
    paths = map (\xs -> startVertex : xs) $ permutations $ map fst $ tail numbers


answer2 :: Grid -> Int
answer2 g = minimum $ map (`distance` edges) paths
  where
    numbers = sortOn snd $ filter (\(_, x) -> isDigit x) $ assocs g
    vertices = map fst numbers
    edges = generateEdges g vertices
    startVertex = fst $ head numbers
    paths = map (\xs -> startVertex : xs ++ [startVertex]) $ permutations $ map fst $ tail numbers

advent24 :: IO ()
advent24 = do
  input <- parseInput <$> getInput 24
  putStrLn $ "Advent 24-1: " ++ show (answer1 input) -- 456
  putStrLn $ "Advent 24-2: " ++ show (answer2 input) -- 704
