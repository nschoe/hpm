module Tools (
               usage
             , hpmFolder
             , libraryFile
             , withLibrary
             , withBook
             , noEntryBook
             , parsingProblem
             ) where

import           Data.Functor ((<$>))
import           System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>))

-- Classic usage command showing commands and their syntax
usage :: String
usage = concat [ "Usage : hpm\t-h, --help : display this message\n"
                       , "\t\t-l, --list : list all entries\n"
                       , "\t\t-i, --init : initiate a new entry book with a new master password\n"
                       , "\t\t-r, --reset : reset your entry book by deleting all stored passwords in it\n"
                       , "\t\t-a, --add <service> <user> : add a new password for service <service> with user <user>, the password will be prompted on a secure shell.\n"
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

