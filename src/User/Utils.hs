{-# LANGUAGE OverloadedStrings #-}
module User.Utils
  (
    hashPassword
  , isVaildPassword
  ) where


import           Crypto.Hash.SHA1      (hash)
import           Crypto.MAC.HMAC       (hmac)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack, unpack)

secretKey :: ByteString
secretKey = "3Jvp4hQ4vb6yfg"

hashPassword :: String -> String
hashPassword msg = concatMap w . unpack $ hmac hash 32 secretKey msg'
  where msg' = pack msg
        w ch = let s = "0123456789ABCDEF"
                   x = fromEnum ch
               in [s !! div x 16,s !! mod x 16]

isVaildPassword :: String -> String -> Bool
isVaildPassword passwd hashStr = hashStr == hashPassword passwd
