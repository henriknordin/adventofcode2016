--- Day 22: Grid Computing ---
{-# LANGUAGE OverloadedStrings #-}
module Advent22 where

import           Text.Megaparsec (someTill, eof, skipManyTill)
import           Text.Megaparsec.Char (space, eol, printChar)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

import           Data.List (foldl', nub)
import           Data.Maybe (fromJust, isJust)

import qualified Data.Map.Strict as Map (Map, insert, member, fromList, keys, (!), delete)
import qualified Data.PQueue.Prio.Min as PQ (MinPQueue, deleteFindMin, empty, insert, singleton)
import qualified Data.Set as S (Set, singleton, notMember, insert)

import           Advent.Megaparsec (Parser, getParsed)
import           Advent.Lib (getInput)


data Coordinate =
  Coordinate { x :: !Int 
             , y :: !Int
             } deriving (Show, Eq, Ord)
data Node =
  Node { coordinate :: !Coordinate
       , size       :: !Int
       , used       :: !Int
       } deriving (Show, Eq)

avail :: Node -> Int
avail n = size n - used n

type Grid = Map.Map Coordinate Node

data Walk =
  Walk { node  :: Node -- Would it be ok if this is just a Coordinate?
       , steps :: !Int
       , hole  :: Maybe Node
       , grid  :: Grid
       } deriving (Show, Eq)

parseInput :: Parser [Node]
parseInput = do
  "root@ebhq-gridcenter# df -h" *> eol
  "Filesystem" *> space *> "Size  Used  Avail  Use%" *> eol
  nodes <- someTill parseNode eof
  pure nodes

parseNode :: Parser Node
parseNode = do
  --"/dev/grid/node-x1-y7     85T   69T    16T   81%"
  x <- "/dev/grid/node-x" *> L.decimal
  y <- "-y" *> L.decimal 
  size <- space *> L.decimal <* "T"
  used <- space *> L.decimal <* "T"
  skipManyTill printChar eol
  pure $ Node (Coordinate x y) size used

answer1 :: [Node] -> Int
answer1 xs = length $ viable xs

viable :: [Node] -> [(Node, Node)]
viable xs = go xs xs
  where 
    go :: [Node] -> [Node] -> [(Node, Node)]
    go [] nodes = []
    go (x:xs) nodes 
      | used x == 0 = go xs nodes
      | otherwise   = map (\n -> (x, n)) (filter (\n -> avail n >= used x && coordinate n /= coordinate x) nodes) ++ go xs nodes


-- START: x = 31, y = 0
answer2 :: Coordinate -- ^ The coordinate that should be reached
        -> [Node]     -- ^ All existing nodes
        -> Int        -- ^ The number of steps to reach the goal
answer2 goal nodes = snd $ fromJust $ aStar w0 predicate (`cost` goal) generator
  where
    -- The grid as it looks at time = 0
    grid0 :: Grid
    grid0 = Map.fromList $ map (\n -> (coordinate n, n)) $ nub $ concatMap (\(n1, n2) -> [n1, n2]) $ viable nodes

    startX = maximum $ map x $ filter (\c -> y c == 0) $ Map.keys grid0
    coordinate0 = Coordinate startX 0
    n0 = grid0 Map.! coordinate0
    w0 = Walk n0 0 (Just hole0) grid0

    predicate :: Node -> Bool
    predicate n = coordinate n == goal

    -- /dev/grid/node-x26-y22   86T    0T    86T    0%
    hole0 :: Node
    hole0 = grid0 Map.! Coordinate 26 22

cost :: Walk -> Coordinate -> Int 
cost (Walk n s _ _) goal = s + heuristic n
  where
    heuristic (Node (Coordinate a b) _ _) = abs (x goal - a) + abs (y goal - b)

-- ^ Generate the next version of the grid give data is moved from
-- ^ current coordinate to next
nextGrid :: Coordinate -- ^ Current coordinate 
         -> Coordinate -- ^ Next coordinate 
         -> Grid       -- ^ The current grid
         -> Grid       -- ^ The next grid
nextGrid c0 c1 g = Map.insert c1 n1' $ Map.insert c0 n0' g
  where 
    n0 = g Map.! c0
    n1 = g Map.! c1
    n0' = Node (coordinate n0) (size n0) (used n1)
    n1' = Node (coordinate n1) (size n1) 0


generator :: Walk     -- ^ The path taken 
          -> [Walk]   -- ^ The next legal steps
generator (Walk n s h g) = map (\(c, g', s') -> Walk (g' Map.! c) (s+s'+1) (Just (g' Map.! coordinate n)) g') 
                         -- Reinsert the coordinate that was removed to walk around data
                         $ map (\(c, (g', s')) -> (c, nextGrid (coordinate n) c (Map.insert (coordinate n) (g Map.! coordinate n) g'), s'))
                         -- Filter out and unpack maybe from hole search
                         $ map (\(x, y) -> (x, fromJust y))
                         $ filter (\(x, m) -> isJust m)
                         -- May in the holes
                         $ map (\c -> (c, moveHole c (Map.delete (coordinate n) g))) 
                         $ nextCoordinates c g
  where
    c = coordinate n
    moveHole :: Coordinate -> Grid -> Maybe (Grid, Int)
    moveHole c g = aStar (Walk (fromJust h) 0 Nothing g) (\(Node nc _ _) -> nc == c) (`cost` c) holeGen

nextCoordinates :: Coordinate -> Grid -> [Coordinate]
nextCoordinates c g = filter (`Map.member` g) 
                    $ filter isBounded 
                    $ map (\(ox, oy) -> Coordinate (x c + ox) (y c + oy)) offsets
  where
    offsets :: [(Int, Int)]
    offsets = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    isBounded :: Coordinate -> Bool
    isBounded (Coordinate x y) = x >= 0 && x <= 31 && y >= 0 && y <= 29 


holeGen :: Walk -> [Walk]
holeGen (Walk n s _ g) = map (\(c, g) -> Walk (g Map.! c) (s+1) Nothing g) 
                       $ map (\c -> (c, nextGrid (coordinate n) c g)) cs
  where
    cs = nextCoordinates (coordinate n) g

aStar :: Walk                 -- ^ The starting point of the walk
      -> (Node -> Bool)       -- ^ The predicate
      -> (Walk -> Int)        -- ^ The cost including heuristics
      -> (Walk -> [Walk])     -- ^ Generator for next possible steps
      -> Maybe (Grid, Int)    -- ^ The minimun number of steps to fulfill predicate
aStar w0 p cost gen = go (PQ.singleton (cost w0) w0) (S.singleton $ coordinate $ node w0)
  where
    go :: PQ.MinPQueue Int Walk -> S.Set Coordinate -> Maybe (Grid, Int)
    go queue visited
      | queue == PQ.empty = Nothing
      | otherwise        =
          let ((_, walk), queue') = PQ.deleteFindMin queue
              neighbours = gen walk
              newNeighbours = filter (\(Walk (Node c _ _) _ _ _) -> S.notMember c visited) neighbours 
              visited' = foldl' (\b a -> S.insert (coordinate $ node a) b) visited newNeighbours
              queue'' = foldl' (\b a -> PQ.insert (cost a) a b) queue' newNeighbours
          in if p (node walk) then Just (grid walk, steps walk) else go queue'' visited'

advent22 :: IO ()
advent22 = do
  input <- getInput 22
  nodes <- getParsed parseInput input
  putStrLn $ "Advent 22-1: " ++ show (answer1 nodes)                  -- 952
  putStrLn $ "Advent 22-2: " ++ show (answer2 (Coordinate 0 0) nodes) -- 181

