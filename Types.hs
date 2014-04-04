module Types (
               PasswordEntry(..)
             , LibraryEntry(..)
             , PwdHash
             , Service
             , User
             , Pwd
             ) where

import qualified Data.ByteString.Lazy as B (ByteString)
import           Data.Csv (FromRecord(..), ToRecord(..), record, toField, (.!))
import qualified Data.Vector as V (length)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad (mzero)

type PwdHash = B.ByteString
type Service = String
type User = String
type Pwd = B.ByteString

-- Represents an entry inside the library book
data LibraryEntry = LibraryEntry {
      getLibraryHash :: PwdHash
    , getLibraryFile :: FilePath
} deriving (Eq, Show)

-- Represents an entry inside an entry book
data PasswordEntry = PasswordEntry {
      getService :: Service
    , getUser :: User
    , getPwd :: Pwd
} deriving (Eq)

-- The Show instance for PasswordEntry doesn't show the password
instance Show PasswordEntry where
    show pwdEntry = "(" ++ getService pwdEntry ++ ", " ++ getUser pwdEntry ++ ")"

-- ToRecord instance to serialize the password entry into Csv
instance ToRecord PasswordEntry where
    toRecord pwdEntry = record [toField (getService pwdEntry), toField (getUser pwdEntry), toField (getPwd pwdEntry)]

-- FromRecord instance for PasswordEntry to parse Csv
instance FromRecord PasswordEntry where
    parseRecord v | V.length v == 3 = PasswordEntry <$> (v .! 0) <*> (v .! 1) <*> (v .! 2)
                  | otherwise = mzero

-- ToRecord instance to serialize the library entry into Csv
instance ToRecord LibraryEntry where
    toRecord libraryEntry = record [toField (getLibraryHash libraryEntry), toField (getLibraryFile libraryEntry)]

-- FromRecord instance for LibraryEntry to parse Csv
instance FromRecord LibraryEntry where
    parseRecord v | V.length v == 2 = LibraryEntry <$> (v .! 0) <*> (v .! 1)
                  | otherwise = mzero
