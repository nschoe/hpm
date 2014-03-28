module BookLibrary (
                    askMasterPwd
                   ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B (fromStrict)
import qualified Data.ByteString.Char8 as B8 (pack)
import System.IO (hFlush, stdout)
import Types

-- Prompts the user for its master password
askMasterPwd :: IO PwdHash
askMasterPwd =
    putStr "Please type your master password : " >> hFlush stdout >> getLine >>= return . B.fromStrict . B8.pack
