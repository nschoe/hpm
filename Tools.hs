module Tools (
               usage
             , hpmFolder
             , libraryFile
             , withLibrary
             , withBook
             , noEntryBook
             , parsingProblem
             , promptUser
             , toClipboard
             ) where

import qualified Data.ByteString.Lazy as B (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.Functor ((<$>))
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Process (runCommand)

-- Classic usage command showing commands and their syntax
usage :: String
usage = concat [ "Usage : hpm\t-h, --help : display this message.\n"
                       , "\t\t-l, --list : list all entries.\n"
                       , "\t\t-i, --init : initiate a new entry book with a new master password.\n"
                       , "\t\t-r, --reset : reset your entry book by deleting all stored passwords in it.\n"
                       , "\t\t-a, --add <service> <user> : add a new password for service <service> with user <user>, the password will be prompted on a secure shell.\n"
                       , "\t\t-d, --delete <service> : delete the stored password for service <service>.\n"
                       , "\t\t-e, --extract <service> : extract a password for service <service>.\n"
               ]

-- Error message displayed when no entry book was found
noEntryBook :: String
noEntryBook = "No entry book associated with that master password was found.\n\
              \Initiate a new entry book with --init, -i or consider reading \
              \the help with --help, -h.\n"

-- Errot to display when 'decode' function fails
parsingProblem :: String
parsingProblem = "There was a problem when parsing your Csv data.\n\
                 \Consider resetting your file (all passwords will be lost)."

-- The hpm folder, containing the library and the entry books
hpmFolder :: IO FilePath
hpmFolder = (</> ".hpm") <$> getHomeDirectory 

-- The file containing the master passwords's hashes and the corresponding entry books
libraryFile :: IO FilePath
libraryFile = (</> ".booklibary") <$> hpmFolder

-- Handles the test to verify that the hpm folder and the library file exist to carry out computation
withLibrary :: (FilePath -> IO a) -> IO a
withLibrary f = do
  hpmFolder >>= createDirectoryIfMissing True
  libraryExists <- libraryFile >>= doesFileExist
  if libraryExists then
      libraryFile >>= f
  else
      libraryFile >>= flip writeFile "" >> libraryFile >>= f

{- Handles the test to verify that the entry book exists to carry out computation.
   Argument should be full path name. -}
withBook :: FilePath -> (FilePath -> IO a) -> IO a
withBook book f = do
  exists <- doesFileExist book
  case exists of
    False -> error "The entry book doesn't exist, you may be looking at data corruption."
    True -> f book

-- Prompts the user for its username (for a specific service)
promptUser :: IO String
promptUser = putStrLn "Type user name : " >> getLine

-- Copy the password into the user's clipboard
toClipboard :: B.ByteString -> IO ()
toClipboard password = do
  let passwordStr = B8.unpack password
  _ <- runCommand $ "echo " ++ passwordStr ++ " | xclip -i"
  putStrLn "Password now in clipboard."
