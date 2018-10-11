module Advent17 (advent17) where

import           Data.Char (ord)
import           Data.Maybe (fromJust, catMaybes)
import           Data.Ord (comparing)
import           Data.List (maximumBy)

import           Advent.Hash (md5hash)
import           Advent.Lib (getInput)

newtype Coordinate = Coordinate (Int, Int)

data Walk = Walk !Coordinate !String

path :: Walk -> String
path (Walk _ p) = p

search :: String           -- ^ The passcode, or inital value to hash
       -> (Walk -> Bool)   -- ^ The predicate
       -> Maybe String     -- ^ The path to reach the goal
search code p = go [Walk (Coordinate (1, 1)) code]
  where
    go :: [Walk] -> Maybe String
    go [] = Nothing
    go (walk:queue) =
      let neighbours = next walk
      in if p walk
           then Just (path walk)
           else go (queue ++ neighbours)

search2 :: String           -- ^ The passcode, or inital value to hash
        -> (Walk -> Bool)   -- ^ The predicate
        -> [Walk]           -- ^ The paths that fulfill the predicate
search2 code p = go [Walk (Coordinate (1, 1)) code]
  where
    go :: [Walk] -> [Walk]
    go [] = []
    go (walk:queue) =
      let neighbours = next walk
      in if p walk
           then walk : go queue
           else go (queue ++ neighbours)

next (Walk (Coordinate (x, y)) path) = filter inside $ catMaybes
  [ if isOpen u then Just (Walk (Coordinate (x, y - 1)) (path ++ "U")) else Nothing
  , if isOpen d then Just (Walk (Coordinate (x, y + 1)) (path ++ "D")) else Nothing
  , if isOpen l then Just (Walk (Coordinate (x - 1, y)) (path ++ "L")) else Nothing
  , if isOpen r then Just (Walk (Coordinate (x + 1, y)) (path ++ "R")) else Nothing
  ]
  where
    (u:d:l:r:_) = take 4 $ md5hash path

inside :: Walk -> Bool
inside (Walk (Coordinate (x, y)) _) = x >= 1 && x <= 4 && y >= 1 && y <= 4

isOpen :: Char -> Bool
isOpen c = ord 'b' <= ord c && ord c <= ord 'f' 

answer1 :: String -> String
answer1 s = drop (length s) $ fromJust $ search s (\(Walk (Coordinate (x, y)) _) -> x == 4 && y == 4)

answer2 :: String -> Int
answer2 s = length 
          $ drop (length s) 
          $ path 
          $ maximumBy (comparing (length . path)) 
          $ search2 s (\(Walk (Coordinate (x, y)) _) -> x == 4 && y == 4)

parseInput :: String -> String
parseInput = head . lines

advent17 :: IO ()
advent17 = do
  input <- parseInput <$> getInput 17
  putStrLn $ "Advent 17-1: " ++ show (answer1 input) -- RRRLDRDUDD
  putStrLn $ "Advent 17-2: " ++ show (answer2 input) -- 706

