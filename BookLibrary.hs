module BookLibrary (
                    askMasterPwd
                   ) where

import System.IO (hFlush, stdout)

-- Prompts the user for its master password
askMasterPwd :: IO String
askMasterPwd =
    putStr "Please type your master password : " >> hFlush stdout >> getLine
