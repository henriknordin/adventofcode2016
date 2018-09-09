--- Day 12: Leonardo's Monorail ---
--
module Advent12
    ( advent12
    ) where

import           Data.Char (ord)
import           Text.Read (readMaybe)
import qualified Data.Sequence as S (Seq, fromList, index, update)

import           Lib (getInput)


newtype Register = 
  Register Int deriving (Show)

unpack :: Register -> Int
unpack (Register x) = x

data Opcode = 
    Cpy (Either Int Register) Register
  | Inc Register 
  | Dec Register
  | Jnz (Either Int Register) Int
  deriving (Show)

parseInput :: String -> [Opcode]
parseInput = map (parseOpcode . words) . lines
  where
    parseOpcode :: [String] -> Opcode
    parseOpcode ["cpy", x, y] = Cpy (parseEither x) (mapRegister y)
    parseOpcode ["jnz", x, y] = Jnz (parseEither x) (read y :: Int)
    parseOpcode ["inc", x] = Inc (mapRegister x)
    parseOpcode ["dec", x] = Dec (mapRegister x)
    parseEither :: String -> Either Int Register
    parseEither xs = 
      let x = readMaybe xs :: Maybe Int
      in case x of
        Just a  -> Left a
        Nothing -> Right (mapRegister xs)
    mapRegister :: String -> Register
    mapRegister c = Register $ ord (head c) - ord 'a'
                    
answer1 :: [Opcode] -> Int
answer1 xs = let register = process (S.fromList [0, 0, 0, 0]) xs
             in S.index register 0

answer2 :: [Opcode] -> Int
answer2 xs = let register = process (S.fromList [0, 0, 1, 0]) xs
             in S.index register 0

process :: S.Seq Int -> [Opcode] -> S.Seq Int
process reg opcodes = go reg opcodes 0
  where
    go :: S.Seq Int -> [Opcode] -> Int -> S.Seq Int
    go reg opcodes pointer
      | pointer >= length opcodes = reg
      | otherwise                 = 
          let opcode = opcodes !! pointer
              (offset, reg') = processOp reg opcode
              nextPointer = offset + pointer
          in go reg' opcodes (offset + pointer)

processOp :: S.Seq Int -> Opcode -> (Int, S.Seq Int)
processOp reg = go
  where
    go (Cpy (Left x) y)  = (1, S.update (unpack y) x reg)
    go (Cpy (Right x) y) = (1, S.update (unpack y) (get x reg) reg)
    go (Inc x)           = (1, S.update (unpack x) (get x reg + 1) reg)
    go (Dec x)           = (1, S.update (unpack x) (get x reg - 1) reg)
    go (Jnz (Left x) y)  = if x /= 0 then (y, reg) else (1, reg)
    go (Jnz (Right x) y) = if get x reg /= 0 then (y, reg) else (1, reg)

get :: Register -> S.Seq Int -> Int
get (Register x) reg = S.index reg x

advent12 :: IO ()
advent12 = do
  input <- parseInput <$> getInput 12
  putStrLn $ "Advent 12-1: " ++ show (answer1 input) -- 318077
  putStrLn $ "Advent 12-2: " ++ show (answer2 input) -- 9227731

