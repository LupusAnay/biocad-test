module Biocad.Database.Utils
  ( nodeExtractValue,
    uRelExtractValue,
    Query (..),
  )
where

import Data.Either.Combinators (mapLeft, maybeToRight)
import Biocad.Error
import qualified Data.Map as M
import qualified Data.Text as T
import Database.Bolt
  ( Node,
    RecordValue,
    URelationship,
    Value,
    exactEither,
    nodeProps,
    urelProps,
  )
import GHC.Generics (Generic)

data Query
  = Query (T.Text, T.Text)
  | QueryP (T.Text, T.Text, M.Map T.Text Value)
  deriving (Show, Generic)

nodeExtractValue :: (RecordValue a) => Node -> T.Text -> Either Error a
nodeExtractValue node = _extractValue (nodeProps node)

uRelExtractValue :: (RecordValue a) => URelationship -> T.Text -> Either Error a
uRelExtractValue rel = _extractValue (urelProps rel)

_extractValue :: (RecordValue a) => M.Map T.Text Value -> T.Text -> Either Error a
_extractValue values key = do
  value <- maybeToRight (DatabaseError . DecodeError  $ PropertyNotFound key) $ M.lookup key values
  mapLeft (\_ -> DatabaseError . DecodeError $ PropertyHasWrongType) $ exactEither value
