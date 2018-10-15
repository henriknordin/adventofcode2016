module Advent.Hash
  ( toMD5
  ) where

import           Crypto.Hash

import qualified Data.ByteString as B (ByteString)
import           Data.String (fromString)

toMD5 :: String -> String
toMD5 = show . md5 . fromString
  where
    md5 :: B.ByteString -> Digest MD5
    md5 = hash

