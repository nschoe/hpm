module Tools (
               usage
             , hpmFolder
             , libraryFile
             , withLibrary
             , noEntryBook
             ) where

import           BookLibrary
import           Control.Monad (liftM, mapM_)
import qualified Data.ByteString.Lazy as B
import           Data.Functor ((<$>))
import           System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>))

-- Classic usage command showing commands and their syntax
usage :: String
usage = concat [ "Usage : hpm\t-h, --help : display this message\n"
                       , "\t\t-l, --list : list all entries\n"
                       , "\t\t-i, --init : initiate a new entry book with a new master password\n"
                       , "\t\t-r, --reset : reset your entry book by deleting all stored passwords in it\n"
               ]

-- Error message displayed when no entry book was found
noEntryBook :: String
noEntryBook = "No entry book associated with that master password was found.\n\
              \Initiate a new entry book with --init, -i or consider reading\
              \the help with --help, -h.\n"

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

{- Handles verification of the master password to carry out computation
   on the contents of the entry book file read as a lazy ByteString -}
{-withPassword :: String -> (B.ByteString -> IO a) -> IO a
withPassword pwd f = withLibrary $ \booklibary -> do
                     libraryRaw <- B.readFile booklibary
                     lookup
-}
