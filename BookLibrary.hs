module BookLibrary (
                     askMasterPwd
                   , askPassword
                   ) where

import qualified Data.ByteString.Lazy as B (fromStrict, ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB8 (pack)
import Types (PwdHash)
import Encrypt (hashPassword)
import System.Console.Haskeline (runInputT, getPassword, defaultSettings)

-- Prompts the user for its master password
askMasterPwd :: IO PwdHash
askMasterPwd = askPassword (Just "Please type your master password : ") >>= return . B.fromStrict . hashPassword

{- Asks the user to type a password, secure the typing in the terminal.
   Optionally passes a prompt message.
-}

askPassword :: Maybe String -> IO (B.ByteString)
{-askPassword Nothing = askPassword'
askPassword (Just message) = putStr message >> hFlush stdout >> askPassword'
   
askPassword' :: IO (B.ByteString)
askPassword' = getLine >>= return . B.fromStrict . B8.pack
-}
askPassword Nothing = askPassword' ""
askPassword (Just msg) = askPassword' msg

askPassword' :: String -> IO (B.ByteString)
askPassword' msg = do
  pwd <- runInputT defaultSettings $ getPassword Nothing msg
  case pwd of
    Nothing   -> error "You must type a password when prompted!\n"
    Just pass -> return (LB8.pack pass)
