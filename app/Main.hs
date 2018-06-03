{-# LANGUAGE NumDecimals #-}
module Main where

import System.IO (readFile)
import System.Environment (getArgs)

import Lib (getInput)

import qualified Advent01 as A01 (parseInput, answer1, answer2)

advent01 :: IO ()
advent01 = do
  input <- A01.parseInput <$> getInput 1
  putStrLn $ "Advent 1-1: " ++ show (A01.answer1 input)  -- 300
  putStrLn $ "Advent 1-2: " ++ show (A01.answer2 input)  -- 159


parse :: [String] -> IO ()
parse ["01"] = advent01
parse _      = advent01 

main :: IO ()
main = getArgs >>= parse

