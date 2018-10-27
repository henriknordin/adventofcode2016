{-# LANGUAGE ScopedTypeVariables #-}
module Advent.Search 
  ( aStar
  ) where

import           Data.List (foldl') 
import qualified Data.PQueue.Prio.Min as PQ (MinPQueue, insert, singleton, minView)
import qualified Data.Set as S (Set, singleton, notMember, insert)


data Vertex a =
  Vertex { label :: !a
         , distance :: !Int -- distance from the root to the vertex
         } deriving (Show, Eq)

-- TODO Define a type for PQ.MinPQueue Int (Vertex a)

aStar :: forall a. (Ord a, Eq a) 
      => a               -- ^ The starting point
      -> (a -> Int)      -- ^ The heuristic cost function
      -> (a -> [a])      -- ^ Next steps
      -> (a -> Bool)     -- ^ The predicate
      -> Maybe (Int, a)  -- ^ The Building matching the predicate
aStar b heuristic next p = go (PQ.singleton (heuristic b) (Vertex b 0)) (S.singleton b)
  where
    go :: PQ.MinPQueue Int (Vertex a) -> S.Set a -> Maybe (Int, a)
    go queue seen = 
      case PQ.minView queue of
        Nothing          -> Nothing
        Just (v, queue') -> 
          if p $ label v 
            then Just (distance v, label v)
            else let vertices = prune seen $ expand v
                 in go (enqueue vertices queue') (addSeen vertices seen)
    
    expand :: Vertex a -> [Vertex a]
    expand v = map (`Vertex` nextDistance) $ next (label v)
      where
        nextDistance = 1 + distance v

    prune :: S.Set a -> [Vertex a] -> [Vertex a]
    prune seen = filter (\x -> S.notMember (label x) seen)

    enqueue :: [Vertex a] -> PQ.MinPQueue Int (Vertex a) -> PQ.MinPQueue Int (Vertex a)
    enqueue vs q = foldl' (\acc v -> PQ.insert (cost v heuristic) v acc) q vs

    addSeen :: [Vertex a] -> S.Set a -> S.Set a
    addSeen vs s = foldl' (\acc v -> S.insert (label v) acc) s vs

cost :: Vertex a -> (a -> Int) -> Int
cost v heuristic = distance v + heuristic (label v)
