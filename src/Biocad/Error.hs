module Biocad.Error
  ( Error (..),
    DatabaseError (..),
    DecodeError (..),
    fromBoltError,
  )
where

import qualified Data.Text as T
import Database.Bolt (BoltError)

data Error = DatabaseError DatabaseError
  deriving (Show)

data DatabaseError
  = DriverError DriverError
  | DecodeError DecodeError
  | EntityNotFound
  | UnknownError T.Text
  deriving (Show)

data DecodeError
  = PropertyNotFound T.Text
  | PropertyHasWrongType
  | RelationHasWrongType {got :: T.Text, expected :: T.Text}
  | NodeDoesNotHaveExpectedLabel {expected :: T.Text}
  | RecordNotFound
  deriving (Show)

newtype DriverError = Internal BoltError
  deriving (Show)

fromBoltError :: BoltError -> Error
fromBoltError = DatabaseError . DriverError . Internal
