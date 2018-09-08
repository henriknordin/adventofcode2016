module Lib
    ( getInput
    , combinations
    ) where

import System.IO (readFile)
import Text.Printf (printf)
import Data.List (tails)

getInput :: Int -> IO String
getInput i = readFile (printf "data/input%02d.txt" i)

combinations :: Integer -> [a] -> [[a]]
combinations 0 lst = [[]]
combinations n lst = do
  (x:xs) <- tails lst
  rest   <- combinations (n - 1) xs
  return $ x : rest
