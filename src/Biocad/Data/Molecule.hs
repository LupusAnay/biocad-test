module Biocad.Data.Molecule (Molecule(..)) where

import Biocad.Data.Utils (Id)
import Data.Text
import GHC.Generics (Generic)

data Molecule
  = Molecule
      { id :: Id,
        smiles :: Text,
        iupacName :: Text
      }
  deriving (Show, Generic)
