module Biocad.Data.Path (Edge (..), Vertex (..), PathSegment (..)) where

import Biocad.Data.Molecule (Molecule)
import Biocad.Data.ProductFrom (ProductFrom)
import Biocad.Data.Reaction (Reaction)
import Biocad.Data.ReagentIn (ReagentIn)

data Edge = Reagent ReagentIn | Product ProductFrom
  deriving (Show)

data Vertex = M Molecule | R Reaction
  deriving (Show)

data PathSegment = E Edge | V Vertex
  deriving (Show)
