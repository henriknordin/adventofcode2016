--- Day 24: Air Duct Spelunking ---
module Advent24 where

import           Data.Array
import           Data.Char (isDigit)
import           Data.List (sortOn, foldl', permutations)
import           Data.Maybe (fromJust, isJust)

import qualified Data.PQueue.Prio.Min as PQ (MinPQueue, deleteFindMin, empty, insert, singleton)
import qualified Data.Set as S (Set, singleton, notMember, insert)

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

data Walk =
  Walk { coordinate :: Coordinate
       , steps  :: Int
       } deriving Eq

aStar :: Walk                 -- ^ The starting point of the walk
      -> (Coordinate -> Bool)     -- ^ The predicate
      -> (Coordinate -> Int)        -- ^ The cost including heuristics
      -> (Walk -> [Walk])     -- ^ Generator for next possible steps
      -> Maybe Int            -- ^ The minimun number of steps to fulfill predicate
aStar w0 p cost gen = go (PQ.singleton (cost $ coordinate w0) w0) (S.singleton $ coordinate w0)
  where
    go :: PQ.MinPQueue Int Walk -> S.Set Coordinate -> Maybe Int
    go queue visited
      | queue == PQ.empty = Nothing
      | otherwise        =
          let ((_, walk), queue') = PQ.deleteFindMin queue
              neighbours = gen walk
              newNeighbours = filter (\(Walk c _) -> S.notMember c visited) neighbours 
              visited' = foldl' (\b a -> S.insert (coordinate a) b) visited newNeighbours
              queue'' = foldl' (\b a -> PQ.insert (steps a + cost (coordinate a)) a b) queue' newNeighbours
          in if p (coordinate walk) then Just (steps walk) else go queue'' visited'

cost :: Coordinate -> Coordinate -> Int
cost (v1x, v1y) (v2x, v2y) = abs (v1x - v2x) + abs (v1y - v2y)

generateEdges :: Grid -> [Vertex] -> [Edge]
generateEdges g vs = map (\(v1, v2, mw) -> (v1, v2, fromJust mw)) 
                   $ filter (\(c1, c2, w) -> isJust w) 
                   $ map (\(c1, c2) -> (c1, c2, aStar (Walk c1 0) (== c2) (`cost` c2) (generator g))) pairs
  where
    pairs = [(v1, v2) | v1 <- vs, v2 <- vs, v1 /= v2]

generator :: Grid -> Walk -> [Walk]
generator g (Walk (x,y) s) = map (\(x,y) -> Walk (x,y) (s+1))
                           $ filter (\(x, y) -> g ! (x, y) /= '#') 
                           $ filter isBounded 
                           $ map (\(ox, oy) -> ((x + ox), (y + oy))) offsets
  where
    offsets :: [(Int, Int)]
    offsets = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    isBounded :: (Int, Int) -> Bool
    isBounded (x, y) = x >= x0 && x <= x1 && y >= y0 && y <= y1
      where
        ((x0,y0),(x1,y1)) = bounds g

-- total number of premutations is the length of numbers minus the starting point
distance :: [(Int, Int)] -> [Edge] -> Int
distance xs edges = go xs 
  where
    go :: [(Int, Int)] -> Int
    go []       = 0
    go (x:[])   = 0
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

