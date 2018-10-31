module Advent11 where

import           Data.List (sort, groupBy, (\\)) 
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Ord (comparing)

import           Advent.Lib (combinations)
import           Advent.Search (aStar)

data Item = Generator !String | Microchip !String
  deriving (Show, Eq)

instance Ord Item where
  compare = comparing word

word :: Item -> String
word (Microchip c) = c
word (Generator c) = c

data Move = 
  Move { direction :: !Int
       , items :: ![Item]
       } deriving (Show, Eq)

type Floor = [Item]

-- Note that the building represents a vertex in the graph
data Building = 
  Building { elevator    :: !Int
           , floors      :: ![Floor]
           } deriving (Show, Eq, Ord)

--  1 item(s)  takes 1 move
--  2 item(s) takes 1 move
--  3 item(s) takes 3 moves
--  4 item(s) takes 5 moves
--  5 item(s) takes 7 moves
--  6 item(s) takes 9 moves
-- 10 item(s) takes 17 moves to move up one floor
--
-- Should return the euclidian distance of moving all items to the top
-- TODO This does not take elevator positioning into account, which in
--      effect understates the number of moves
heuristic :: Building -> Int
heuristic b = go 0 (floors b)
  where
    go _ [_] = 0
    go found (x:xs) =
      let items = length x
          totalItems = found + items
          floorCost = if totalItems < 3 
                        then if totalItems == 0 then 0 else 1 
                        else 2 * (totalItems - 2) + 1
      in floorCost + go (found + items) xs


isLegal :: Floor -> Bool
isLegal xs = 
  null xs || -- if nothing is on the floor valid by default
  not (any isGenerator xs) || -- if no generator it's also safe
  not (not (null chips)  && not (null gens))
  where
   unpaired = oddItems xs
   chips = filter isMicrochip unpaired
   gens  = filter isGenerator xs

oddItems :: Floor -> Floor
oddItems xs = concat $ filter (\a -> length a == 1) 
                     $ groupBy (\a b -> word a == word b) xs

isMicrochip :: Item -> Bool
isMicrochip (Microchip _) = True
isMicrochip _             = False

isGenerator :: Item -> Bool
isGenerator = not . isMicrochip

next :: Building -> [Building]
next b = mapMaybe (nextBuilding b) moves
  where
    moves = nextMoves b

nextBuilding :: Building -> Move -> Maybe Building
nextBuilding (Building e fs) m = 
  if isLegal (fs' !! e) && isLegal (fs' !! nextLvl)
    then Just (Building nextLvl fs')
    else Nothing
  where
    nextLvl    = e + direction m
    fs' = zipWith update indices fs
    update :: Int -> Floor -> Floor
    update e' f
      | e' == e       = f \\ items m
      | e' == nextLvl = sort $ f ++ items m
      | otherwise     = f

indices :: [Int]
indices = [0..]

nextMoves :: Building -> [Move]
nextMoves (Building e floors) = 
  [Move d i | i <- itemCombos
            , d <- [-1, 1]
            , e + d >= 0 && e + d <= 3
            , validItems i]
  where
    floor = floors !! e
    itemCombos = combinations 1 floor ++ combinations 2 floor

validItems :: [Item] -> Bool
validItems [Generator a, Microchip b] = a == b
validItems [Microchip a, Generator b] = a == b
validItems _                          = True

-- The number of moves to get all to the top, relaxing the condition that
-- chips can burn but keeping that one is needed to power the elevator, 
-- results in 27. The cost function used in A* should reflect this, i.e.
-- the minimum euclidian distance.
input1 :: [Floor]
input1 =
  [ sort [Generator "PR", Microchip "PR"]
  , sort [Generator "CO", Generator "CU", Generator "RU", Generator "PL"]
  , sort [Microchip "CO", Microchip "CU", Microchip "RU", Microchip "PL"]
  , []
  ]

input2 :: [Floor]
input2 =
  [ sort [Generator "EL", Microchip "EL", Generator "DI", Microchip "DI", Generator "PR", Microchip "PR"]
  , sort [Generator "CO", Generator "CU", Generator "RU", Generator "PL"]
  , sort [Microchip "CO", Microchip "CU", Microchip "RU", Microchip "PL"]
  , []
  ]

benchmark :: [Floor]
benchmark =
  [ sort [Generator "DI", Microchip "DI", Generator "PR", Microchip "PR"]
  , sort [Generator "CO", Generator "CU", Generator "RU", Generator "PL"]
  , sort [Microchip "CO", Microchip "CU", Microchip "RU", Microchip "PL"]
  , []
  ]

done :: Building -> Int -> Bool
done (Building _ fs) total = length top == total
  where 
    top = fs !! 3

answer :: [Floor] -> Int
answer xs = fst $ fromJust result
  where
    items = (length . concat) xs
    result = aStar (Building 0 xs) heuristic next (`done` items) id

advent11 :: IO ()
advent11 = do
  putStrLn $ "Advent 11-1: " ++ show (answer input1) -- 33
  putStrLn $ "Advent 11-2: " ++ show (answer input2) -- 57

