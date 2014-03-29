module BookLibrary (
                     askMasterPwd
                   , askPassword
                   ) where

import qualified Data.ByteString.Lazy as B (fromStrict)
import qualified Data.ByteString.Char8 as B8 (pack)
import System.IO (hFlush, stdout)
import Types

-- Prompts the user for its master password
askMasterPwd :: IO PwdHash
askMasterPwd = askPassword (Just "Please type your master password : ")

{- Asks the user to type a password, secure the typing in the terminal.
   Optionally passes a prompt message.
-}
askPassword :: Maybe String -> IO PwdHash
askPassword Nothing = askPassword'
askPassword (Just message) = putStr message >> hFlush stdout >> askPassword'
   
askPassword' :: IO PwdHash
askPassword' = getLine >>= return . B.fromStrict . B8.pack
