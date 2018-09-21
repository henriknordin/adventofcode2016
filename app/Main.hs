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
import           Advent07 (advent07)
import           Advent08 (advent08)
import           Advent09 (advent09)
import           Advent10 (advent10)
import           Advent11 (advent11)
import           Advent12 (advent12)
import           Advent13 (advent13)
import           Advent14 (advent14)
import           Advent15 (advent15)
import           Advent16 (advent16)
import           Advent17 (advent17)
import           Advent18 (advent18)
import           Advent19 (advent19)
import           Advent20 (advent20)

parse :: [String] -> IO ()
parse ["01"] = advent01
parse ["02"] = advent02
parse ["03"] = advent03
parse ["04"] = advent04
parse ["05"] = advent05
parse ["06"] = advent06
parse ["07"] = advent07
parse ["08"] = advent08
parse ["09"] = advent09
parse ["10"] = advent10
parse ["11"] = advent11
parse ["12"] = advent12
parse ["13"] = advent13
parse ["14"] = advent14
parse ["15"] = advent15
parse ["16"] = advent16
parse ["17"] = advent17
parse ["18"] = advent18
parse ["19"] = advent19
parse ["20"] = advent20
parse _      = advent01
            >> advent02
            >> advent03
            >> advent04
            >> advent05
            >> advent06
            >> advent07
            >> advent08
            >> advent09
            >> advent10
            >> advent11
            >> advent12
            >> advent13
            >> advent14
            >> advent15
            >> advent16
            >> advent17
            >> advent18
            >> advent19
            >> advent20

main :: IO ()
main = getArgs >>= parse

