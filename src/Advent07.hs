-- Day 7: Internet Protocol Version 7
--
module Advent07
    ( advent07
    ) where

import           Data.List (elemIndex, span)
import           Data.Maybe (fromJust)
import           Lib (getInput)

advent07 :: IO ()
advent07 = do
  input <- parseInput <$> getInput 7
  putStrLn $ "Advent 7-1: " ++ show (answer1 input)  -- 115
  putStrLn $ "Advent 7-2: " ++ show (answer2 input)  -- 231

parseInput :: String -> [String]
parseInput = lines

answer1 :: [String] -> Int
answer1 = length . filter (== True) . map hasTls

answer2 :: [String] -> Int 
answer2 = length . filter (== True) . map hasSsl

hasTls:: String -> Bool
hasTls xs = let tokens = tokenize xs
                posTokens = filter (\s -> head s /= '[') tokens
                negTokens = filter (\s -> head s == '[') tokens
            in  any abba posTokens && (not . any abba) negTokens
  where
    abba :: String -> Bool
    abba (a:b:c:d:xs) = (a == d && b == c && a /= b) || abba (b:c:d:xs)
    abba _            = False

hasSsl :: String -> Bool
hasSsl xs = let tokens = tokenize xs
                posTokens = filter (\s -> head s /= '[') tokens
                negTokens = filter (\s -> head s == '[') tokens
                abas = concatMap aba posTokens
                babs = map bab abas
            in any (`findBab` babs) negTokens
  where
    aba :: String -> [String]
    aba (a:b:c:xs) = if a == c && a /= b
                       then [a, b, c] : aba (b:c:xs)
                       else aba (b:c:xs)
    aba _          = []
    
    bab :: String -> String
    bab [a, b, _] = [b, a, b]

    findBab :: String -> [String] -> Bool
    findBab (a:b:c:xs) babs = let s = [a, b, c]
                              in s `elem` babs || findBab (b:c:xs) babs
    findBab _ _             = False
                        
-- poor implementation -- cannot handle test6
-- perhaps nicer to map into Either and use lefts/rights
-- instead of pos/neg tokens
tokenize :: String -> [String]
tokenize xs = let (s1, s2) = span (/= '[') xs
                  endIndex = fromJust $ elemIndex ']' s2
              in case s2 of 
                [] -> [s1]
                _  -> s1 : take (endIndex + 1) s2 : tokenize (drop (endIndex + 1) s2)

test1 = "abba[mnop]qrst"
test2 = "abcd[bddb]xyyx"
test3 = "aaaa[qwer]tyui"
test4 = "ioxxoj[asdfgh]zxcvbn"
test5 = "ioxxoj[asddsgh]zxcvbn"
test6 = "ioxxoj[asddsgh]"
test = [test1, test2, test3, test4, test5]

ssl1 = "aba[bab]xyz"
ssl2 = "xyx[xyx]xyx"
ssl3 = "aaa[kek]eke"
ssl4 = "zazbz[bzb]cdb"

