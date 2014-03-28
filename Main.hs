module Main (
              main
            , libraryLookup
            ) where

import           BookLibrary
import qualified Data.ByteString.Lazy as B
import           Data.Csv hiding (lookup)
import qualified Data.Vector as V
import           System.Environment (getArgs)
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
  masterPwd <- askMasterPwd
  putStrLn $ "<DEBUG> Init " ++ masterPwd
initiate _ libraryFile = putStrLn "Answer with \"yes\" or \"no\"." >> initiate Nothing libraryFile

-- Lookup generalized for the library book to be used with 'withLibrary'
{-
  TODO : use withLibrary inside that function rather than reading the file like this
-}
libraryLookup :: PwdHash -> FilePath -> IO (Maybe FilePath)
libraryLookup pwd libraryFile = do
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
