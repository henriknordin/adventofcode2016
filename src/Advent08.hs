-- Day 8: Two-Factor Authentication
--
{-# LANGUAGE OverloadedStrings #-}
module Advent08
    ( advent08
    ) where

import           Data.Array (Array, array, bounds, (//), (!))
import           Data.Foldable (toList)

import           Text.Megaparsec (eof, someTill, (<|>)) 
import           Text.Megaparsec.Char (eol)
import qualified Text.Megaparsec.Char.Lexer  as L (decimal)

import           Advent.Lib (Parser, parseWith)


type Grid = Array (Int, Int) Int

data Rule = Rect Int Int
          | RotateRow Int Int
          | RotateCol Int Int
          deriving (Show)

ruleParser :: Parser [Rule]
ruleParser = do
  rules <- someTill (parseRect <|> parseRotRow <|> parseRotCol) eof
  pure rules
  where
    parseRect = do
      r <- "rect " *> L.decimal
      c <- "x" *> L.decimal <* eol
      pure (Rect r c)
    parseRotRow = do
      y <- "rotate row y=" *> L.decimal
      s <- " by " *> L.decimal <* eol
      pure (RotateRow y s)
    parseRotCol = do
      x <- "rotate column x=" *> L.decimal
      s <- " by " *> L.decimal <* eol
      pure (RotateCol x s)

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

pretty :: Grid -> String
pretty g = unlines $ map (unwords . map (show . (g !))) indices
  where indices = [[(x, y) | x <- [startX..endX]] | y <- [startY..endY]]
        ((startX, startY), (endX, endY)) = bounds g

answer1 :: [Rule] -> Int
answer1 = length
        . filter (== 1) 
        . toList 
        . foldl process (mkGrid 50 6)

answer2 :: [Rule] -> String
answer2 = pretty . foldl process (mkGrid 50 6)

advent08 :: IO ()
advent08 = do
  rules <- parseWith ruleParser 8
  putStrLn $ "Advent 8-1: " ++ show (answer1 rules)  -- 121
  putStrLn   "Advent 8-2: read the below text as 6x5 bitmaps" 
  putStr $ answer2 rules -- RURUCEOEIL

