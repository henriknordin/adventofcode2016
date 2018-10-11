{-# LANGUAGE OverloadedStrings #-}
-- Day 10: Balance Bots
--
module Advent10
    ( advent10
    ) where

import           Text.Megaparsec (some, (<|>)) 
import qualified Text.Megaparsec.Char.Lexer  as L (decimal)
import           Data.List (find)
import qualified Data.Map as M

import           Advent.Megaparsec (Parser, getParsed)
import           Advent.Lib (getInput)

advent10 :: IO ()
advent10 = do
  input <- getInput 10
  instr <- getParsed parseInput input
  putStrLn $ "Advent 10-1: " ++ show (answer1 instr (61, 17)) -- 161
  putStrLn $ "Advent 10-2: " ++ show (answer2 instr)  -- 133163

newtype BotId = BotId Int deriving (Eq, Ord, Show)

data Instruction 
  = Assign BotId Int
  | Action BotId (Either BotId Int) (Either BotId Int)
  deriving (Show)
 
data Bot = Bot BotId [Int] deriving (Show)

parseInput :: Parser [Instruction]
parseInput = some (parseAssign <|> parseAction)

parseAssign :: Parser Instruction
parseAssign = do
  v <- "value " *> L.decimal
  i <- " goes to bot " *> L.decimal <* "\n"
  pure (Assign (BotId i) v)

parseAction :: Parser Instruction
parseAction = do
  i <- "bot " *> L.decimal <* " gives low to "
  low  <- parseDest
  high <- " and high to " *> parseDest <* "\n"
  pure (Action (BotId i) low high)

parseDest :: Parser (Either BotId Int)
parseDest = (Left . BotId <$> ("bot " *> L.decimal)) <|> (Right <$> ("output " *> L.decimal))

isAction :: Instruction -> Bool
isAction Action{} = True
isAction _        = False

answer1 :: [Instruction] -> (Int, Int) -> Int
answer1 xs pair = let bots = mkBots xs
                      regs = M.empty
                      (Bot (BotId botId) _) = go bots regs (filter isAction xs)
                  in botId
  where
    go :: M.Map BotId Bot -> M.Map Int Int -> [Instruction] -> Bot
    go bots _   []  = error "Combination not found" 
    go bots r (x:xs) = case travFind bots pair of
                         Just bot -> bot
                         _        -> 
                           let res = process bots r x
                           in case res of 
                             Just (b, rs') -> go b rs' xs
                             _      -> go bots r (xs ++ [x])

answer2 :: [Instruction] -> Int
answer2 xs = let bots = mkBots xs
                 regs = go bots M.empty (filter isAction xs)
             in product $ map (\k -> regs M.! k) [0, 1, 2] 
  where
    go :: M.Map BotId Bot -> M.Map Int Int -> [Instruction] -> M.Map Int Int
    go bots r   []   = r
    go bots r (x:xs) = let res = process bots r x
                       in case res of 
                         Just (b, rs') -> go b rs' xs
                         _      -> go bots r (xs ++ [x])


travFind :: M.Map BotId Bot -> (Int, Int) -> Maybe Bot
travFind m pair = let bots = M.elems m
                      xs = [fst pair, snd pair]
                      sx = reverse xs
                in find (\(Bot botId cargo) -> cargo == xs || cargo == sx) bots

process :: M.Map BotId Bot -> M.Map Int Int -> Instruction -> Maybe (M.Map BotId Bot, M.Map Int Int)
process m r (Action k (Right l) (Right h)) = do
  bot <- M.lookup k m
  hilo' <- hilo bot
  let r' = M.insert l (fst hilo') r
  let r'' = M.insert h (snd hilo') r'
  return (m, r'')
process m r (Action k (Left l) (Right h)) = do
  bot <- M.lookup k m
  let botLo = M.findWithDefault (Bot l []) l m
  hilo' <- hilo bot
  loCargo <- botCargo botLo
  let m' = M.delete (bId bot) m
  let m'' = M.insert l (Bot l  (fst hilo':loCargo)) m' 
  let r' = M.insert h (snd hilo') r
  return (m'', r')
process m r (Action k (Right l) (Left h)) = do
  bot <- M.lookup k m
  let botHi = M.findWithDefault (Bot h []) h m
  hilo' <- hilo bot
  hiCargo <- botCargo botHi
  let m' = M.delete (bId bot) m
  let m'' = M.insert h (Bot h  (snd hilo':hiCargo)) m' 
  let r' = M.insert l (fst hilo') r
  return (m'', r')
process m r (Action k (Left l) (Left h)) = do
  bot <- M.lookup k m
  let botLo = M.findWithDefault (Bot l []) l m
  let botHi = M.findWithDefault (Bot h []) h m
  hilo' <- hilo bot
  loCargo <- botCargo botLo
  hiCargo <- botCargo botHi
  let m' = M.delete (bId bot) m
  let m'' = M.insert l (Bot l  (fst hilo':loCargo)) m' 
  let m''' = M.insert h (Bot h  (snd hilo':hiCargo)) m'' 
  return (m''', r)

--  where 
hilo :: Bot -> Maybe (Int, Int)
hilo (Bot _ xs) 
  | length xs == 2 = Just (minimum xs, maximum xs)
  | otherwise      = Nothing

bId :: Bot -> BotId
bId (Bot i _) = i
botCargo :: Bot -> Maybe [Int]
botCargo (Bot _ xs)
  | length xs <= 1 = Just xs
  | otherwise      = Nothing


mkBots :: [Instruction] -> M.Map BotId Bot
mkBots = foldl mkBot M.empty

mkBot :: M.Map BotId Bot -> Instruction -> M.Map BotId Bot 
mkBot m (Assign k x) = 
  let bot = M.lookup k m
  in case bot of 
    Just (Bot _ xs) -> M.insert k (Bot k (x:xs)) m
    Nothing         -> M.insert k (Bot k [x]) m
mkBot m _              = m

