module Main where

import System.IO (readFile)
import System.Environment (getArgs)

import Lib (getInput)

import           Advent01 (advent01)
import           Advent02 (advent02)
import           Advent03 (advent03)
import           Advent04 (advent04)
import           Advent05 (advent05)
import           Advent06 (advent06)

parse :: [String] -> IO ()
parse ["01"] = advent01
parse ["02"] = advent02
parse ["03"] = advent03
parse ["04"] = advent04
parse ["05"] = advent05
parse ["06"] = advent06
parse _      = advent01
            >> advent02
            >> advent03
            >> advent04
            >> advent05
            >> advent06

main :: IO ()
main = getArgs >>= parse

