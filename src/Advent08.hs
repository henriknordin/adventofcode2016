{-# LANGUAGE OverloadedStrings #-}
-- Day 8: Two-Factor Authentication
--
module Advent08
    ( advent08
    ) where

import           Data.Array (Array, array, bounds, range, (//), (!))
import           Data.List.Split (chunksOf)
import           Data.Foldable (toList)
import           Lib (getInput)

import           Text.Megaparsec (Parsec, many, parse, parseErrorPretty, (<|>)) 
import qualified Text.Megaparsec.Char.Lexer  as L (decimal)
import           Data.Void


advent08 :: IO ()
advent08 = do
  input <- getInput 8
  rules <- getParsed parseInput input
  putStrLn $ "Advent 8-1: " ++ show (answer1 rules)  -- 121
  putStrLn   "Advent 8-2: read the below text as 6x5 bitmaps" 
  putStr $ answer2 rules -- RURUCEOEIL

type Grid = Array (Int, Int) Int

data Rule = Rect Int Int
          | RotateRow Int Int
          | RotateCol Int Int
          deriving (Show)

type Parser = Parsec Void String

getParsed :: Parser a -> String -> IO a
getParsed p s = case parse p "dummy.txt" s of
            Left err -> fail (parseErrorPretty err)
            Right a  -> return a

parseInput :: Parser [Rule]
parseInput = do
  rules <- many parseRule
  pure rules

parseRule :: Parser Rule
parseRule = parseRect <|> parseRotRow <|> parseRotCol

parseRect :: Parser Rule
parseRect = do
  r <- "rect " *> L.decimal
  c <- "x" *> L.decimal <* "\n"
  pure (Rect r c)

parseRotRow :: Parser Rule
parseRotRow = do
  y <- "rotate row y=" *> L.decimal
  s <- " by " *> L.decimal <* "\n"
  pure (RotateRow y s)

parseRotCol:: Parser Rule
parseRotCol = do
  x <- "rotate column x=" *> L.decimal
  s <- " by " *> L.decimal <* "\n"
  pure (RotateCol x s)

answer1 :: [Rule] -> Int
answer1 = length
        . filter (== 1) 
        . toList 
        . foldl process (mkGrid 50 6)

answer2 :: [Rule] -> String
answer2 = pretty' . foldl process (mkGrid 50 6)

process :: Grid -> Rule -> Grid
process g (Rect a b) = rect g a b
process g (RotateCol a b) = rotateCol g a b
process g (RotateRow a b) = rotateRow g a b

mkGrid :: Int -> Int -> Grid
mkGrid sizeX sizeY = array ((0, 0), (cols, rows)) [((c, r), 0) | c <- [0..cols], r <- [0..rows]]
 where 
   cols = sizeX - 1
   rows = sizeY - 1

rect :: Grid -> Int -> Int -> Grid
rect g cols rows = g // [((c, r), 1) | c <- [0..cols-1], r <- [0..rows-1]]

rotateCol :: Grid -> Int -> Int -> Grid
rotateCol g c shift = g // [((c, (r + shift) `mod` (rows + 1)), g!(c, r)) | r <- [0..rows]]
  where
    ((_,_), (_,rows)) = bounds g

-- rotate row y=A by B shifts all of the pixels in row A 
-- (0 is the top row) right by B pixels. Pixels that would 
-- fall off the right end appear at the left end of the row.
rotateRow :: Grid -> Int -> Int -> Grid
rotateRow g r shift = g // [(((c + shift) `mod` (cols + 1), r), g!(c,r))| c <- [0..cols]]
  where
    ((_,_), (cols,_)) = bounds g

pretty :: Grid -> IO ()
pretty g = putStr . unlines . chunksOf xBound . map (\c -> if c == 0 then '.' else '#') . toList $ g
  where
    xBound = fst . snd . bounds $ g

pretty' :: Grid -> String
pretty' g = unlines $ map (unwords . map (show . (g !))) indices
  where indices = [[(x, y) | x <- [startX..endX]] | y <- [startY..endY]]
        ((startX, startY), (endX, endY)) = bounds g

pretty'' :: Grid -> String
pretty'' g = unlines $ map (show . (g !)) indices
  where indices = range $ bounds g

g0 = mkGrid 7 3
g1 = rect g0 3 2
g2 = rotateCol g1 1 1
g2' = rotateRow g1 1 1
g3 = rotateRow g2 0 4

