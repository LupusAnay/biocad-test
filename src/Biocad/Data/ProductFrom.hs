module Biocad.Data.ProductFrom (ProductFrom(..)) where

import GHC.Generics (Generic)

newtype ProductFrom
  = ProductFrom
      { amount :: Double
      }
  deriving (Generic, Show)
