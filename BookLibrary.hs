module BookLibrary (
                     askMasterPwd
                   , askPassword
                   ) where

import qualified Data.ByteString.Lazy as B (fromStrict, ByteString)
import qualified Data.ByteString.Char8 as B8 (pack)
import System.IO (hFlush, stdout)
import Types (PwdHash)
import Encrypt (hashPassword)

-- Prompts the user for its master password
askMasterPwd :: IO PwdHash
askMasterPwd = askPassword (Just "Please type your master password : ") >>= return . B.fromStrict . hashPassword

{- Asks the user to type a password, secure the typing in the terminal.
   Optionally passes a prompt message.
-}
askPassword :: Maybe String -> IO (B.ByteString)
askPassword Nothing = askPassword'
askPassword (Just message) = putStr message >> hFlush stdout >> askPassword'
   
askPassword' :: IO (B.ByteString)
askPassword' = getLine >>= return . B.fromStrict . B8.pack
