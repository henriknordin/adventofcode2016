{-# LANGUAGE NumDecimals #-}
module Main where

import System.IO (readFile)
import System.Environment (getArgs)

import Lib (getInput)

import qualified Advent01 as A01 (parseInput, answer1, answer2)
import qualified Advent02 as A02 (parseInput, answer1, answer2)

advent01 :: IO ()
advent01 = do
  input <- A01.parseInput <$> getInput 1
  putStrLn $ "Advent 1-1: " ++ show (A01.answer1 input)  -- 300
  putStrLn $ "Advent 1-2: " ++ show (A01.answer2 input)  -- 159

advent02 :: IO ()
advent02 = do
  input <- A02.parseInput <$> getInput 2
  putStrLn $ "Advent 2-1: " ++ show (A02.answer1 input)  -- 24862
  putStrLn $ "Advent 2-2: " ++ show (A02.answer2 input)  -- 46C91


parse :: [String] -> IO ()
parse ["01"] = advent01
parse ["02"] = advent02
parse _      = advent01
            >> advent02

main :: IO ()
main = getArgs >>= parse

