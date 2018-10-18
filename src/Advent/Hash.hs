module Advent.Hash
  ( toMD5
  , toMd5Raw
  ) where

import           Crypto.Hash

import qualified Data.ByteString as B (ByteString)
import           Data.String (fromString)

import           Data.ByteArray.Encoding

toMD5 :: String -> String
toMD5 = show . md5 . fromString
  where
    md5 :: B.ByteString -> Digest MD5
    md5 = hash

toMd5Raw :: B.ByteString -> B.ByteString
toMd5Raw = convertToBase Base16 . md5
  where
    md5 :: B.ByteString -> Digest MD5
    md5 = hash

