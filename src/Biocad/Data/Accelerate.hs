module Biocad.Data.Accelerate (Accelerate (..)) where

import GHC.Generics (Generic)

data Accelerate
  = Accelerate
      { temperature :: Float,
        pressure :: Float
      }
  deriving (Show, Generic)
