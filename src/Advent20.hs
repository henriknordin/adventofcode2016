--- Day 20: Firewall Rules ---
{-# LANGUAGE OverloadedStrings #-}
module Advent20 where

import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Text.Megaparsec (Parsec, many, parse, parseErrorPretty) 
import qualified Text.Megaparsec.Char.Lexer  as L (decimal)
import           Data.Void

import           Lib (getInput)

data Blacklist =
  Blacklist { lower :: !Int
            , upper :: !Int
            } deriving (Show)

type Parser = Parsec Void String

getParsed :: Parser a -> String -> IO a
getParsed p s = case parse p "dummy.txt" s of
            Left err -> fail (parseErrorPretty err)
            Right a  -> return a

answer1 :: [Blacklist] -> Int
answer1 = firstAllowed . merge

firstAllowed :: [Blacklist] -> Int
firstAllowed (x:y:xs) = if upper x < lower y then upper x + 1 else firstAllowed (y:xs)
          
answer2 :: [Blacklist] -> Int
answer2 xs = (2 ^ 32) - blocked (merge xs)

blocked :: [Blacklist] -> Int
blocked [] = 0
blocked (x:xs) = interval + blocked xs
  where
    interval = upper x - lower x + 1

merge :: [Blacklist] -> [Blacklist]
merge xs = go $ sortBy (comparing lower) xs
  where
    go [] = []
    go [x] = [x]
    go (x:y:xs) = if upper x + 1 >= lower y
                    then merge (Blacklist (lower x) (max (upper x) (upper y)) : xs)
                    else x : merge (y:xs)
          
parseInput :: Parser [Blacklist]
parseInput = do
  lists <- many parseBlacklist
  pure lists 

parseBlacklist = do
  l <- "" *> L.decimal
  h <- "-" *> L.decimal <* "\n"
  pure (Blacklist l h)

advent20 :: IO ()
advent20 = do
  input <- getInput 20
  lists <- getParsed parseInput input
  putStrLn $ "Advent 20-1: " ++ show (answer1 lists) -- 17348574
  putStrLn $ "Advent 20-2: " ++ show (answer2 lists) -- 104

