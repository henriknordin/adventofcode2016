--- Day 22: Grid Computing ---
{-# LANGUAGE OverloadedStrings #-}
module Advent22 where

import           Text.Megaparsec (someTill, eof, skipManyTill)
import           Text.Megaparsec.Char (space, eol, printChar)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

import           Data.List (nub)
import           Data.Maybe (fromJust, isJust)

import qualified Data.Map.Strict as Map (Map, insert, member, fromList, keys, (!), delete)

import           Advent.Search (aStar)
import           Advent.Lib (Parser, parseWith)


data Coordinate =
  Coordinate { x :: !Int 
             , y :: !Int
             } deriving (Show, Eq, Ord)
data Node =
  Node { coordinate :: !Coordinate
       , size       :: !Int
       , used       :: !Int
       } deriving (Show, Eq, Ord)

avail :: Node -> Int
avail n = size n - used n

type Grid = Map.Map Coordinate Node

data Walk =
  Walk { node  :: Node -- Would it be ok if this is just a Coordinate?
       , steps :: !Int
       , hole  :: Node
       , grid  :: Grid
       } deriving (Show, Eq, Ord)

nodesParser :: Parser [Node]
nodesParser = do
  "root@ebhq-gridcenter# df -h" *> eol
  "Filesystem" *> space *> "Size  Used  Avail  Use%" *> eol
  nodes <- someTill nodeParser eof
  pure nodes

nodeParser :: Parser Node
nodeParser = do
  --"/dev/grid/node-x1-y7     85T   69T    16T   81%"
  x <- "/dev/grid/node-x" *> L.decimal
  y <- "-y" *> L.decimal 
  size <- space *> L.decimal <* "T"
  used <- space *> L.decimal <* "T"
  skipManyTill printChar eol
  pure $ Node (Coordinate x y) size used

viable :: [Node] -> [(Node, Node)]
viable xs = go xs xs
  where 
    go :: [Node] -> [Node] -> [(Node, Node)]
    go [] nodes = []
    go (x:xs) nodes 
      | used x == 0 = go xs nodes
      | otherwise   = map (\n -> (x, n)) (filter (\n -> avail n >= used x && coordinate n /= coordinate x) nodes) ++ go xs nodes


cost :: Walk -> Coordinate -> Int 
cost (Walk n s _ _) goal = s + heuristic n
  where
    heuristic (Node (Coordinate a b) _ _) = abs (x goal - a) + abs (y goal - b)

heuristic :: (Node, Grid) -> Coordinate -> Int 
heuristic (Node (Coordinate a b) _ _, g) goal = abs (x goal - a) + abs (y goal - b)

-- ^ Generate the next version of the grid give data is moved from
-- ^ current coordinate to next
nextGrid :: Coordinate -- ^ Current coordinate 
         -> Coordinate -- ^ Next coordinate 
         -> Grid       -- ^ The current grid
         -> Grid       -- ^ The next grid
nextGrid c0 c1 g0 = Map.insert c1 n1' $ Map.insert c0 n0' g0
  where 
    n0 = g0 Map.! c0
    n1 = g0 Map.! c1
    n0' = Node (coordinate n0) (size n0) (used n1)
    n1' = Node (coordinate n1) (size n1) 0


generator :: Walk     -- ^ The path taken 
          -> [Walk]   -- ^ The next legal steps
generator (Walk n s h g) = map (\(c, g', s') -> Walk (g' Map.! c) (s+s') (g' Map.! coordinate n) g') 
                         -- Reinsert the coordinate that was removed to walk around data
                         $ map (\(c, (s', (_, g'))) -> (c, nextGrid (coordinate n) c (Map.insert (coordinate n) (g Map.! coordinate n) g'), s'))
                         -- Filter out and unpack maybe from hole search
                         $ map (\(x, m) -> (x, fromJust m))
                         $ filter (\(x, m) -> isJust m)
                         -- May in the holes
                         $ map (\c -> (c, moveHole c (Map.delete (coordinate n) g))) 
                         $ nextCoordinates c g
  where
    c = coordinate n
    moveHole :: Coordinate -> Grid -> Maybe (Int, (Node, Grid))
    moveHole c g = aStar (h, g) 
                         (`heuristic` c) 
                         holeGen
                         (\(Node nc _ _, _) -> nc == c) 
                         (\(n, _) -> coordinate n)

nextCoordinates :: Coordinate -> Grid -> [Coordinate]
nextCoordinates c g = filter (`Map.member` g) 
                    $ filter isBounded 
                    $ map (\(ox, oy) -> Coordinate (x c + ox) (y c + oy)) offsets
  where
    offsets :: [(Int, Int)]
    offsets = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    isBounded :: Coordinate -> Bool
    isBounded (Coordinate x y) = x >= 0 && x <= 31 && y >= 0 && y <= 29 


holeGen :: (Node, Grid) -> [(Node, Grid)]
holeGen (n, g) = map (\(c, g) -> ((g Map.! c), g)) 
               $ map (\c -> (c, nextGrid (coordinate n) c g)) cs
  where
    cs = nextCoordinates (coordinate n) g


answer1 :: [Node] -> Int
answer1 xs = length $ viable xs

-- START: x = 31, y = 0
answer2 :: Coordinate -- ^ The coordinate that should be reached
        -> [Node]     -- ^ All existing nodes
        -> Int        -- ^ The number of steps to reach the goal
answer2 goal nodes = (\(s, w) -> s + steps w) 
                   $ fromJust 
                   $ aStar w0 
                           (`cost` goal) 
                           generator 
                           predicate 
                           (coordinate . node)
  where
    -- The grid as it looks at time = 0
    grid0 :: Grid
    grid0 = Map.fromList $ map (\n -> (coordinate n, n)) $ nub $ concatMap (\(n1, n2) -> [n1, n2]) $ viable nodes

    startX = maximum $ map x $ filter (\c -> y c == 0) $ Map.keys grid0
    coordinate0 = Coordinate startX 0
    n0 = grid0 Map.! coordinate0
    w0 = Walk n0 0 hole0 grid0

    predicate :: Walk -> Bool
    predicate n = coordinate (node n) == goal

    -- /dev/grid/node-x26-y22   86T    0T    86T    0%
    hole0 :: Node
    hole0 = grid0 Map.! Coordinate 26 22

advent22 :: IO ()
advent22 = do
  nodes <- parseWith nodesParser 22
  putStrLn $ "Advent 22-1: " ++ show (answer1 nodes)                  -- 952
  putStrLn $ "Advent 22-2: " ++ show (answer2 (Coordinate 0 0) nodes) -- 181

