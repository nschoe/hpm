module Main (
              main
            , libraryLookup
            ) where

import           BookLibrary
import           Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as B
import           Data.Csv hiding (lookup)
import qualified Data.Vector as V
import           System.Environment (getArgs)
import           System.IO (openTempFile, hClose)
import           Tools
import           Types

main :: IO ()
main = getArgs >>= go
    where go [] = putStrLn $ "Wrong number of arguments.\n" ++ usage -- no argument : what do to ?
          go ("--help":_) = putStrLn usage
          go ("-h":_) = go ["--help"]
          go ("--list":_) = undefined
          go ("-l":_) = go ["--list"]
          go ("--init":_) = withLibrary (initiate Nothing)
          go ("-i":_) = go ["--init"]

-- Prompt the user for a new master password and create his associated entry book
initiate :: Maybe String -> FilePath -> IO ()
initiate Nothing libraryFile = do
  putStrLn $ "You are going to create a new, empty entry book, are you sure you want to continue ? (yes/no)"
  answer <- getLine
  initiate (Just answer) libraryFile
initiate (Just "no") _ = putStrLn "Initialization aborded, library book left untouched."
initiate (Just "yes") libraryFile = do
  -- Ask user for his master pwd, check that it doesn't already exist and generate a random filename for it
  masterPwd <- askMasterPwd
  pwdExist <- libraryLookup masterPwd
  case pwdExist of
    Just _ -> putStrLn "There already exists a library file associated with that master password.\n\
                       \Consider re-using or chose another master password."
    Nothing -> do
      (filename, h) <- hpmFolder >>= flip openTempFile "bookEntry"
      withLibrary $ \libraryFile -> do
                  let libEntry = encode [LibraryEntry masterPwd filename]
                  B.appendFile libraryFile libEntry
      hClose h
      putStrLn "New library book initiated. You can start storing passwords now."
  
initiate _ libraryFile = putStrLn "Answer with \"yes\" or \"no\"." >> initiate Nothing libraryFile

-- Lookup generalized for the library book to be used with 'withLibrary'
libraryLookup :: PwdHash -> IO (Maybe FilePath)
libraryLookup pwd = withLibrary $ \libraryFile -> do
                      libraryRaw <- B.readFile libraryFile
                      let libraryDec = decode NoHeader libraryRaw :: Either String (V.Vector LibraryEntry)
                      case libraryDec of
                        Left _ -> error $ "Error while trying to read the library, file may be corrupted\n"
                        Right library -> return $ entryLookup pwd (V.toList library)

-- lookup like function that applies to LibraryEntry data type
entryLookup :: PwdHash -> [LibraryEntry] -> Maybe FilePath
entryLookup _ [] = Nothing
entryLookup pwdHash (x:xs) = if (getLibraryHash x) == pwdHash then
                                 Just (getLibraryFile x)
                             else
                                 entryLookup pwdHash xs
