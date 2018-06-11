-- Day 4: Security Through Obscurity
--
module Advent04
    ( advent04
    ) where

import Data.Char (isAlpha, isDigit)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing, Down(Down))

import Lib (getInput)

advent04 :: IO ()
advent04 = do
  input <- parseInput <$> getInput 4
  putStrLn $ "Advent 4-1: " ++ show (answer1 input)  -- 173787
  putStrLn $ "Advent 4-2: " ++ show (answer2 input)  -- 548

data Room = 
  Room { name :: String
       , sector :: Int
       , checksum :: String
       } deriving (Show)

parseInput :: String -> [Room]
parseInput = map parseRoom . lines

parseRoom :: String -> Room
parseRoom s = let (name, s') = break isDigit s
                  (sector, s'') = break (== '[') s'
              in Room name (read sector :: Int) (filter isAlpha s'')

isRealRoom :: Room -> Bool
isRealRoom r = getChecksum (name r) == checksum r

getChecksum :: String -> String
getChecksum =  fiveChecksums . sortTuple . map (\s -> (head s, length s)) . group . sort . filter isAlpha
  where
    -- Possible to sort only on second since the strings are sorted before group 
    sortTuple = sortBy (comparing (Down . snd))
    fiveChecksums = map fst . take 5

answer1 :: [Room] -> Int
answer1 = sum . map sector . filter isRealRoom

answer2 :: [Room] -> Int
answer2  = snd . head . filter (\x -> fst x == "northpole object storage ") . map decryptName

decryptName :: Room -> (String, Int)
decryptName r = (map (\c -> decryptChar c (sector r)) $ name r, sector r)

decryptChar :: Char -> Int -> Char
decryptChar c rot = let start = fromEnum 'a'
                        range = fromEnum 'z' - start + 1
                        i = fromEnum c
                in if c == '-' then ' ' else toEnum (((i - start + rot) `mod` range)+start)

test1 :: String
test1 = 
  "aaaaa-bbb-z-y-x-123[abxyz]\n\
  \a-b-c-d-e-f-g-h-987[abcde]\n\
  \not-a-real-room-404[oarel]\n\
  \totally-real-room-200[decoy]"

test2 :: String
test2 = "qzmt-zixmtkozy-ivhz-343"

