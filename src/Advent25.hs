--- Day 25: Clock Signal ---
{-# LANGUAGE OverloadedStrings #-}
module Advent25 where

import           Data.Char (ord)

import           Data.Void
import           Text.Megaparsec (Parsec, parse, someTill, eof, parseErrorPretty, (<|>)) 
import           Text.Megaparsec.Char (eol, printChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L (space, signed, decimal, skipLineComment, skipBlockComment)

import qualified Data.Sequence as S (Seq, fromList, index, update)
import           Control.DeepSeq

import           Lib (getInput)

newtype Pointer = 
  Pointer Int deriving (Show)

unpack :: Pointer -> Int
unpack (Pointer x) = x

data Opcode = 
    Cpy (Either Int Pointer) !Pointer
  | Inc !Pointer 
  | Dec !Pointer
  | Jnz (Either Int Pointer) (Either Int Pointer)
  | Tgl !Pointer
  | Out (Either Int Pointer)
  | Noop
  deriving (Show)

data Program = 
  Program { opcodes :: S.Seq Opcode 
          , pointer :: !Int
          } deriving (Show)


type Parser = Parsec Void String

getParsed :: Parser a -> String -> IO a
getParsed p s = case parse p "dummy.txt" s of
            Left err -> fail (parseErrorPretty err)
            Right a  -> return a

parseInput :: Parser [Opcode]
parseInput = do
  opcodes <- someTill parseOpcode eof
  pure opcodes

parseOpcode :: Parser Opcode
parseOpcode = parseCpy <|> parseInc <|> parseDec <|> parseJnz <|> parseTgl <|> parseOut
  where
    parseCpy = do
      v1 <- "cpy " *> parseEither
      v2 <- " " *> printChar <* eol
      pure (Cpy v1 (toPointer v2))
    parseInc = do
      v1 <- "inc " *> printChar <* eol
      pure (Inc (toPointer v1))
    parseDec = do
      v1 <- "dec " *> printChar <* eol
      pure (Dec (toPointer v1))
    parseJnz = do
      v1 <- "jnz " *> parseEither
      v2 <- " " *> parseEither <* eol
      pure (Jnz v1 v2)
    parseTgl = do
      v1 <- "tgl " *> printChar <* eol
      pure (Tgl (toPointer v1))
    parseOut = do
      v1 <- "out " *> parseEither <* eol
      pure (Out v1)
    parseEither :: Parser (Either Int Pointer)
    parseEither = (Left <$> parseSigned) <|> (Right <$> (toPointer <$> printChar))
   
    parseSigned = L.signed spaceConsumer L.decimal
    -- TODO Research if we can get signed decimals in a cleaner way
    spaceConsumer :: Parser ()
    spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
      
    toPointer c = Pointer $ ord c - ord 'a'

process :: S.Seq Int -> [Opcode] -> [Int]
process reg ops = go reg (Program (S.fromList ops) 0)
  where
    go :: S.Seq Int -> Program -> [Int]
    go reg program 
      | pointer program >= length (opcodes program) = []
      | otherwise                 = 
          let opcode = opcodes program `S.index` pointer program
          in case opcode of
            Out (Left x)  -> x  : go reg (step 1 program)
            Out (Right x) -> get x reg : go reg (step 1 program)
            _             -> let (program', reg') = processOp program reg opcode
                             -- TODO How can we get rid of this deepseq
                             in reg' `deepseq` go reg' program' 

step :: Int -> Program -> Program
step x (Program ops p) = Program ops (p + x)

processOp :: Program -> S.Seq Int -> Opcode -> (Program, S.Seq Int)
processOp program reg op = go op
  where
    go :: Opcode -> (Program, S.Seq Int)
    go (Inc x)                   = (step 1 program, S.update (unpack x) (get x reg + 1) reg)
    go (Dec x)                   = (step 1 program, S.update (unpack x) (get x reg - 1) reg)
    go (Cpy (Left x) y)          = (step 1 program, S.update (unpack y) x reg)
    go (Cpy (Right x) y)         = (step 1 program, S.update (unpack y) (get x reg) reg)
    go (Jnz (Left x) (Left y))   = if x /= 0 then (step y program, reg) else (step 1 program, reg)
    go (Jnz (Left x) (Right y))  = if x /= 0 then (step (get y reg) program, reg) else (step 1 program, reg)
    go (Jnz (Right x) (Left y))  = if get x reg /= 0 then (step y program, reg) else (step 1 program, reg)
    go (Jnz (Right x) (Right y)) = if get x reg /= 0 then (step (get y reg) program, reg) else (step 1 program, reg)
    go Noop                      = (step 1 program, reg)
--    go (Tgl _)                   = (toogle op reg program, reg)

get :: Pointer -> S.Seq Int -> Int
get (Pointer x) reg = S.index reg x

initRegister :: Int -> S.Seq Int
initRegister x = S.fromList [x, 0, 0, 0]

answer1 :: [Opcode] -> Int
answer1 ops = go 0
  where 
    go :: Int -> Int
    go x = let output = take 1000 $ process (initRegister x) ops
           in if take 1000 (cycle [0, 1]) == output
             then x
             else go (x+1)

advent25 :: IO ()
advent25 = do
  input <- getInput 25
  opcodes <- getParsed parseInput input
  putStrLn $ "Advent 25-1: " ++ show (answer1 opcodes)  -- 189

