module Advent.Megaparsec
  ( Parser
  , getParsed
  ) where

import           Text.Megaparsec (Parsec, parse, parseErrorPretty)
import           Data.Void

type Parser = Parsec Void String

getParsed :: Parser a -> String -> IO a
getParsed p s = case parse p "dummy.txt" s of
            Left err -> fail (parseErrorPretty err)
            Right a  -> return a

