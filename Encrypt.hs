{-# LANGUAGE OverloadedStrings #-}

module Encrypt (
                 hashPassword
               , encrypt
               , decrypt
              ) where

import Codec.Crypto.SimpleAES (IV, crypt, Direction(..), Mode(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B (ByteString, toStrict, filter, notElem)


-- Hash a (master) password with SHA512
hashPassword :: B.ByteString -> ByteString
--hashPassword = hash SHA512ibrary 
hashPassword = B.toStrict -- enough for now : the library book is encrypted, so master passwords never appear in clear

-- Key for encrypting with SimpleAES
aesKey :: ByteString
aesKey = "1234567890123456"

myIV :: IV
myIV = "qwertyuiopasdfgh"

-- Encrypt an entry book's contents
encrypt :: B.ByteString -> IO (B.ByteString)
--encrypt = encryptMsg CBC aesKey
encrypt msg = return $ crypt CBC aesKey myIV  Encrypt msg

-- Decrypt an entry book's contents
decrypt :: B.ByteString -> B.ByteString
--decrypt = decryptMsg CBC aesKey
decrypt msg = B.filter (`B.notElem` "\NUL") $ crypt CBC aesKey myIV Decrypt msg
