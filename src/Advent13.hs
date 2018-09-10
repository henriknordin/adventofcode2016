--- Day 13: A Maze of Twisty Little Cubicles ---
module Advent13 where

import           Data.List (foldl')
import           Data.Maybe (fromJust)
import qualified Data.PQueue.Min as Q (MinQueue, empty, singleton, deleteFindMin, getMin, insert)
import qualified Data.PQueue.Prio.Min as PQ (MinPQueue, deleteFindMin, empty, insert, getMin, singleton)
import           Data.Ord (comparing)
import qualified Data.Set as S (Set, singleton, notMember, insert, size)
import           Numeric (showIntAtBase)
import           Data.Char (intToDigit)

import           Lib (getInput)

-- TODO Try with this as hashable instead
data Coordinate
  = Coordinate { x :: !Int
               , y :: !Int
               } deriving (Show, Eq, Ord)

data Walk
  = Walk { coordinate :: !Coordinate
         , steps      :: !Int
         } deriving (Show, Eq)

equation :: Int -> Coordinate -> Int
equation magic (Coordinate x y) = x * x + 3 * x + 2 * x * y + y + y * y + magic

isOpen :: Int -> Bool
isOpen = even . length . filter (== '1') . toBinary
  where
    toBinary :: Int -> String
    toBinary x = showIntAtBase 2 intToDigit x ""

start :: Walk
start = Walk (Coordinate 1 1) 0

aStar :: (Coordinate -> Bool) -- ^ The predicate
      -> (Walk -> Int)        -- ^ The cost including heuristics
      -> (Walk -> [Walk])     -- ^ Generator for next possible steps
      -> Maybe Int            -- ^ The minimun number of steps to fulfill predicate
aStar p cost gen = go (PQ.singleton (cost start) start) (S.singleton $ coordinate start)
  where
    go :: PQ.MinPQueue Int Walk -> S.Set Coordinate -> Maybe Int
    go queue visited
      | queue == PQ.empty = Nothing
      | otherwise        =
          let ((_, walk), queue') = PQ.deleteFindMin queue
              neighbours = gen walk
              visited' = S.insert (coordinate walk) visited
              newNeighbours = filter (\(Walk c _) -> S.notMember c visited') neighbours 
              queue'' = foldl' (\b a -> PQ.insert (cost a) a b) queue' newNeighbours
          in if p (coordinate walk) then Just (steps walk) else go queue'' visited'


next :: Walk -> [Walk]
next (Walk (Coordinate x y) s) = 
  [Walk (Coordinate (x + offsetX) (y + offsetY)) (s + 1) | offsetX <- [-1, 0, 1]
                                                         , offsetY <- [-1, 0, 1]
                                                         , x + offsetX >= 0 && y + offsetY >= 0
                                                         , (offsetX /= 0 && offsetY == 0) || (offsetX == 0 && offsetY /= 0)]

bfs :: (Int -> Bool)
    -> Int
    -> S.Set Coordinate
bfs p magic = go [start] (S.singleton $ coordinate start)
  where
    go :: [Walk] -> S.Set Coordinate -> S.Set Coordinate
    go [] visited           = visited
    go (walk:queue) visited = 
      let neighbours = filter (isOpen . equation magic . coordinate) $ next walk
          newNeighbours = filter (\(Walk c _) -> S.notMember c visited) neighbours 
          visited' = foldl' (\b a -> S.insert (coordinate a) b) visited newNeighbours
      in if p (steps walk) then visited else go (queue ++ newNeighbours) visited'    

parseInput :: String -> Int
parseInput = read

answer1 :: Int -> Int -> Int -> Int
answer1 goalX goalY magic = fromJust $ aStar predicate cost generator
  where
    magicEquation = equation magic
    predicate (Coordinate x y) = x == goalX && y == goalY
    cost :: Walk -> Int 
    cost (Walk c s) = s + heuristic c
      where
        heuristic (Coordinate x y) = abs (goalX - x) + abs (goalY - y)
    generator :: Walk -> [Walk]
    generator = filter (isOpen . equation magic . coordinate) . next
        

answer2 :: Int -> Int
answer2 magic = S.size $ bfs (== 50) magic

advent13 :: IO ()
advent13 = do
  input <- parseInput <$> getInput 13
  putStrLn $ "Advent 13-T: " ++ show (answer1 7 4 10)      -- 11
  putStrLn $ "Advent 13-1: " ++ show (answer1 31 39 input) -- 92
  putStrLn $ "Advent 13-2: " ++ show (answer2 input)       -- 124

