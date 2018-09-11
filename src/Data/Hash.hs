module Data.Hash
  ( md5hash
  , md5hash'
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Char8 as C
import           Data.Digest.Pure.MD5 (md5, md5DigestBytes)

import           Crypto.Hash

md5hash :: String ->  String
md5hash s = show hashed
  where
    unhashed = C.pack s
    hashed = md5 $ BL.fromStrict unhashed

-- Advent 14-1: 29374
-- Advent 14-1: 38881
-- stack exec adventofcode2016-exe 14  1451.88s user 1652.42s system 503% cpu 10:16.54 total
md5hash' :: String -> String
md5hash' s = show (hashWith MD5 $ C.pack s)
