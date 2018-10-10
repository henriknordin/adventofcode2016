--- Day 15: Timing is Everything ---
{-# LANGUAGE OverloadedStrings #-}
module Advent15 where

import           Data.List (group)
import           Text.Megaparsec (Parsec, many, parse, parseErrorPretty) 
import qualified Text.Megaparsec.Char.Lexer  as L (decimal)
import           Data.Void

import           Lib (getInput)

data Disc =
  Disc { discId   :: !Int
       , size     :: !Int
       , position :: !Int
       } deriving (Show)

type Parser = Parsec Void String

getParsed :: Parser a -> String -> IO a
getParsed p s = case parse p "dummy.txt" s of
            Left err -> fail (parseErrorPretty err)
            Right a  -> return a

parseInput :: Parser [Disc]
parseInput = do
  rules <- many parseDisc
  pure rules

parseDisc :: Parser Disc
parseDisc = do
  i <- "Disc #" *> L.decimal
  s <- " has " *> L.decimal
  j <- " positions; at time=0, it is at position " *> L.decimal <* ".\n"
  pure (Disc i s j)

-- Transform the disc to times it would be possible 
-- to drop the disc in order for it to fall through
transform :: Disc -> [Int]
transform  d = filter (> 0) $ map (\t -> size d - mod (position d) (size d) + t * size d - discId d) [0, 1..]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                        then x:merge xs (y:ys)
                        else y:merge (x:xs) ys

answer1 :: [Disc] -> Int
answer1 xs = head . head . filter (\x -> length x == discs) . group $ merged
  where
    discs = length xs
    txs = map transform xs
    merged = foldr merge [] txs

advent15 :: IO ()
advent15 = do
  input <- getInput 15
  discs <- getParsed parseInput input
  putStrLn $ "Advent 15-1: " ++ show (answer1 discs) -- 203660
  putStrLn $ "Advent 15-2: " ++ show (answer1 (Disc 7 11 0:discs)) -- 2408135
