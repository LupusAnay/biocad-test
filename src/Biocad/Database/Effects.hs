module Biocad.Database.Effects (MonadDB (..)) where

import Biocad.Database.Utils (Query (..))
import Biocad.Error
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Generics.Labels ()
import Database.Bolt (RecordValue)

class (MonadIO m, MonadError Error m) => MonadDB m where
  runQuery :: (RecordValue a) => Query -> m [a]
