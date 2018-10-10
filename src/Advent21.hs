--- Day 21: Scrambled Letters and Hash ---
{-# LANGUAGE OverloadedStrings #-}
module Advent21 (advent21) where

import           Data.Tuple (swap)
import           Data.Sequence
import           Data.Maybe (fromJust)
import           Data.Foldable (toList)
import qualified Data.List as L (reverse)

import           Data.Void
import           Text.Megaparsec (Parsec, many, parse, parseErrorPretty, (<|>)) 
import qualified Text.Megaparsec.Char.Lexer as L (decimal, charLiteral)

import           Prelude hiding (length, reverse, splitAt)

import           Lib (getInput)

data Opcode =
    SwapPos !Int !Int
  | SwapChar !Char !Char
  | RotateL !Int
  | RotateR !Int
  | Rotate !Char
  | Reverse !Int !Int
  | Move !Int !Int
  deriving (Show)

type Parser = Parsec Void String

getParsed :: Parser a -> String -> IO a
getParsed p s = case parse p "dummy.txt" s of
            Left err -> fail (parseErrorPretty err)
            Right a  -> return a

parseInput :: Parser [Opcode]
parseInput = do
  lists <- many parseOpcode
  pure lists
  where
    parseOpcode = parseSwapPos <|> 
                  parseSwapChar <|>
                  parseRotateOneStepL <|>
                  parseRotateL <|>
                  parseRotateOneStepR <|>
                  parseRotateR <|>
                  parseRotate <|>
                  parseReverse <|>
                  parseMove

-- swap position X with position Y
parseSwapPos :: Parser Opcode
parseSwapPos = do
  x <- "swap position " *> L.decimal <* " with position"
  y <- " " *> L.decimal <* "\n"
  pure $ SwapPos x y

swapPos :: Seq a -> Int -> Int -> Seq a
swapPos xs ai bi = update bi a (update ai b xs)
  where
    a = index xs ai
    b = index xs bi

-- swap letter X with letter Y
parseSwapChar :: Parser Opcode
parseSwapChar = do
  x <- "swap letter " *> L.charLiteral <* " with letter"
  y <- " " *> L.charLiteral <* "\n"
  pure $ SwapChar x y

swapChar :: Eq a => Seq a -> a -> a -> Seq a
swapChar xs a b = update bi a (update ai b xs)
  where
    ai = fromJust $ a `elemIndexL` xs
    bi = fromJust $ b `elemIndexL` xs

-- rotate left/right X steps
parseRotateL :: Parser Opcode
parseRotateL = do
  x <- "rotate left " *> L.decimal <* " steps\n"
  pure $ RotateL x

parseRotateOneStepL :: Parser Opcode
parseRotateOneStepL = do
  "rotate left 1 step\n"
  pure $ RotateL 1

rotateLeft :: Int -> Seq a -> Seq a
rotateLeft n xs = (uncurry (><) . swap . splitAt (n `mod` length xs)) xs

parseRotateR :: Parser Opcode
parseRotateR = do
  x <- "rotate right " *> L.decimal <* " steps\n"
  pure $ RotateR x

parseRotateOneStepR :: Parser Opcode
parseRotateOneStepR = do
  "rotate right 1 step\n"
  pure $ RotateR 1

rotateRight :: Int -> Seq a -> Seq a
rotateRight n xs = uncurry (><) . swap $ splitAt (length xs - (n `mod` length xs)) xs                                                                                                                                           

-- rotate based on position of letter X
parseRotate :: Parser Opcode
parseRotate = do
  x <- "rotate based on position of letter " *> L.charLiteral <* "\n"
  pure $ Rotate x

rotateLetter :: Eq a => a -> Seq a -> Seq a
rotateLetter c xs = rotateRight (i+1) xs
  where
    idx = fromJust $ c `elemIndexL` xs
    i   = --traceShow idx $ 
          if idx >= 4 then idx + 1 else idx

rotateLetter' :: Eq a => a -> Seq a -> Seq a
rotateLetter' c xs = rotateLeft i xs
  where
    idx = fromJust $ c `elemIndexL` xs
    i   = case idx of
            1 -> 1
            3 -> 2
            5 -> 3
            7 -> 4
            2 -> 6
            4 -> 7
            6 -> 0
            0 -> 1

-- reverse positions X through Y
parseReverse :: Parser Opcode
parseReverse = do
  x <- "reverse positions " *> L.decimal 
  y <- " through " *> L.decimal <* "\n"
  pure $ Reverse x y

reverseS :: Int -> Int -> Seq a -> Seq a
reverseS a b xs = front >< reverse mid >< end
  where 
    (front, rest) = splitAt a xs
    (mid, end) = splitAt (b+1-a) rest

-- move position X to position Y
parseMove :: Parser Opcode
parseMove = do
  x <- "move position " *> L.decimal 
  y <- " to position " *> L.decimal <* "\n"
  pure $ Move x y

moveS :: Int -> Int -> Seq a -> Seq a
moveS a b xs = insertAt b c (deleteAt a xs) 
  where
    c = index xs a

scramble s (SwapPos a b) = swapPos s a b
scramble s (SwapChar a b) = swapChar s a b
scramble s (RotateL a) = rotateLeft a s
scramble s (RotateR a) = rotateRight a s
scramble s (Rotate a) = rotateLetter a s
scramble s (Reverse a b) = reverseS a b s
scramble s (Move a b) = moveS a b s

descramble s (SwapPos a b) = swapPos s b a
descramble s (SwapChar a b) = swapChar s b a
descramble s (RotateL a) = rotateRight a s
descramble s (RotateR a) = rotateLeft a s
descramble s (Rotate a) = rotateLetter' a s
descramble s (Reverse a b) = reverseS a b s
descramble s (Move a b) = moveS b a s

answer1 :: Seq Char -> [Opcode] -> String
answer1 s xs = toList $ foldl scramble s xs

answer2 :: Seq Char -> [Opcode] -> String
answer2 s xs = toList $ foldl descramble s (L.reverse xs)

advent21 :: IO ()
advent21 = do
  input <- getInput 21
  opcodes <- getParsed parseInput input
  putStrLn $ "Advent 21-1: " ++ show (answer1 "abcdefgh" opcodes) -- ghfacdbe
  putStrLn $ "Advent 21-T: " ++ show (answer2 "ghfacdbe" opcodes) -- should be abcdefgh
  putStrLn $ "Advent 21-2: " ++ show (answer2 "fbgdceah" opcodes) -- fhgcdaeb

