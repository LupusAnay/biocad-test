module Biocad.Data.Reaction (Reaction(..)) where

import Biocad.Data.Utils (Id)
import Data.Text (Text)
import GHC.Generics (Generic)

data Reaction
  = Reaction
      { id :: Id,
        name :: Text
      }
  deriving (Show, Generic)
