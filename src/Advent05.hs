-- Day 5: How About a Nice Game of Chess?
--
{-# LANGUAGE OverloadedStrings #-}
module Advent05
    ( advent05
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B (fromStrict)
import qualified Data.ByteString.Char8 as C
import           Data.Char (digitToInt)
import           Data.Digest.Pure.MD5
import           Data.List (sortBy, nubBy)
import           Data.Ord (comparing)

import           Advent.Lib (getInput)

advent05 :: IO ()
advent05 = do
  input <- parseInput <$> getInput 5
  let generated = genPassword input
  putStrLn $ "Advent 5-1: " ++ show (answer1 generated)  -- f97c354d
  putStrLn $ "Advent 5-2: " ++ show (answer2 generated)  -- 863dde27

parseInput :: String -> B.ByteString
parseInput = C.pack . head . lines

answer1 :: [(Char, Char)] -> String
answer1  = take 8 . map fst

answer2 :: [(Char, Char)] -> String
answer2  = map snd . sortBy (comparing fst) . take 8 . nubBy (\a b -> fst a == fst b) . filter (\c -> fst c < 8) . map (\c -> (digitToInt . fst $ c, snd c))

genPassword :: B.ByteString -> [(Char, Char)]
genPassword = go 0
  where
    go :: Int -> B.ByteString -> [(Char, Char)]
    go i s = let hashed = md5hash s i
             in case valid hashed of
               Nothing -> go (i+1) s
               Just c  -> c : go (i+1) s

valid :: String -> Maybe (Char, Char)
valid s = let prefix = take 5 s
           in if all (== '0') prefix
                then Just (s !! 5, s !! 6)
                else Nothing

md5hash :: B.ByteString -> Int -> String
md5hash s i = let unhashed = B.append s $ C.pack $ show i
                  hashed = md5 $ B.fromStrict unhashed
              in show hashed

