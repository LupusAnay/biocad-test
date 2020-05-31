module Biocad.Data.Catalyst (Catalyst(..)) where

import Biocad.Data.Utils (Id)
import Data.Text (Text)

data Catalyst
  = Catalyst
      { id :: Id,
        smiles :: Text,
        name :: Maybe Text
      }
