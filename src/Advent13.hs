--- Day 13: A Maze of Twisty Little Cubicles ---
module Advent13 where

import           Data.List (foldl')
import           Data.Maybe (fromJust)
import qualified Data.Set as S (Set, singleton, notMember, insert, size)
import           Numeric (showIntAtBase)
import           Data.Char (intToDigit)

import           Advent.Search (aStar)
import           Advent.Lib (getInput)

-- TODO Try with this as hashable instead
data Coordinate
  = Coordinate { x :: !Int
               , y :: !Int
               } deriving (Show, Eq, Ord)

equation :: Int -> Coordinate -> Int
equation magic (Coordinate x y) = x * x + 3 * x + 2 * x * y + y + y * y + magic

isOpen :: Int -> Bool
isOpen = even . length . filter (== '1') . toBinary
  where
    toBinary :: Int -> String
    toBinary x = showIntAtBase 2 intToDigit x ""

start :: Coordinate
start = Coordinate 1 1

next :: Coordinate -> [Coordinate]
next (Coordinate x y) =
  [Coordinate (x + offsetX) (y + offsetY) | offsetX <- [-1, 0, 1]
                                          , offsetY <- [-1, 0, 1]
                                          , x + offsetX >= 0 && y + offsetY >= 0
                                          , (offsetX /= 0 && offsetY == 0) || (offsetX == 0 && offsetY /= 0)]

data Vertex =
  Vertex { label    :: !Coordinate
         , distance :: !Int -- distance from the root to the vertex
         } deriving (Show, Eq)

bfs :: (Int -> Bool)
    -> Int
    -> S.Set Coordinate
bfs p magic = go [Vertex start 0] (S.singleton start)
  where
    go :: [Vertex] -> S.Set Coordinate -> S.Set Coordinate
    go [] visited           = visited
    go (walk:queue) visited = 
      if p (distance walk)
        then visited
        else let vertices = prune visited $ expand walk -- go (queue
             in go (queue ++ vertices) (addSeen vertices visited)
--      let neighbours = filter (isOpen . equation magic . coordinate) $ next walk
--          newNeighbours = filter (`S.notMember`  visited) neighbours 
--          visited' = foldl' (\b a -> S.insert (coordinate a) b) visited newNeighbours
--      in if p (steps walk) then visited else go (queue ++ newNeighbours) visited'    

    expand :: Vertex -> [Vertex]
    expand v = map (`Vertex` nextDistance) 
             $ filter (isOpen . equation magic) 
             $ next (label v)
      where
        nextDistance = 1 + distance v

    prune :: S.Set Coordinate -> [Vertex] -> [Vertex]
    prune seen = filter (\x -> S.notMember (label x) seen)

    addSeen :: [Vertex] -> S.Set Coordinate -> S.Set Coordinate
    addSeen vs s = foldl' (\acc v -> S.insert (label v) acc) s vs

parseInput :: String -> Int
parseInput = read

answer1 :: Int -> Int -> Int -> Int
answer1 goalX goalY magic = fst $ fromJust $ aStar start heuristic generator predicate
  where
    predicate (Coordinate x y) = x == goalX && y == goalY

    heuristic :: Coordinate -> Int 
    heuristic (Coordinate x y) = abs (goalX - x) + abs (goalY - y)
    
    generator :: Coordinate -> [Coordinate]
    generator = filter (isOpen . equation magic) . next
        

answer2 :: Int -> Int
answer2 magic = S.size $ bfs (== 50) magic

advent13 :: IO ()
advent13 = do
  input <- parseInput <$> getInput 13
  putStrLn $ "Advent 13-1: " ++ show (answer1 31 39 input) -- 92
  putStrLn $ "Advent 13-2: " ++ show (answer2 input)       -- 124

