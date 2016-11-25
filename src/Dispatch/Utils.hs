{-# LANGUAGE OverloadedStrings #-}
module Dispatch.Utils
  (
    hashPassword
  , isVaildPassword
  ) where


import           Crypto.Hash.SHA1      (hash)
import           Crypto.MAC.HMAC       (hmac)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Hex              (hex)

secretKey :: ByteString
secretKey = pack "3Jvp4hQ4vb6yfg"

hashPassword :: String -> String
hashPassword msg = unpack $ hex $ hmac hash 32 secretKey msg'
  where msg' = pack msg

isVaildPassword :: String -> String -> Bool
isVaildPassword passwd hashStr = hashStr == hashPassword passwd
