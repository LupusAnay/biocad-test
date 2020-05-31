module Biocad.Database.Decoders
  ( moleculeDecoder,
    reactionDecoder,
    edgeDecoder,
    vertexDecoder,
    pathDecoder,
  )
where

import Biocad.Data
  ( Edge (..),
    Molecule (..),
    PathSegment (..),
    ProductFrom (..),
    Reaction (..),
    ReagentIn (..),
    Vertex (..),
  )
import Biocad.Database.Utils (nodeExtractValue, uRelExtractValue)
import Biocad.Error (DatabaseError (..), DecodeError (..), Error (..))
import Data.List (transpose)
import qualified Data.Text as T
import Database.Bolt (Node, Path (..), URelationship, labels, urelType)

moleculeDecoder :: Node -> Either Error Molecule
moleculeDecoder n =
  Molecule <$> nodeExtractValue n "id"
    <*> nodeExtractValue n "smiles"
    <*> nodeExtractValue n "iupacName"

reactionDecoder :: Node -> Either Error Reaction
reactionDecoder n = Reaction <$> nodeExtractValue n "id" <*> nodeExtractValue n "name"

edgeDecoder :: URelationship -> Either Error Edge
edgeDecoder rel
  | relType == tReagent = pure $ Reagent ReagentIn
  | relType == tProduct = Product . ProductFrom <$> uRelExtractValue rel "amount"
  | otherwise =
    Left . DatabaseError $
      DecodeError
        RelationHasWrongType
          { got = relType,
            expected = T.concat [tReagent, " or ", tProduct]
          }
  where
    relType = urelType rel
    tReagent = "REAGENT_IN"
    tProduct = "PRODUCT_FROM"

vertexDecoder :: Node -> Either Error Vertex
vertexDecoder n
  | haveLabel lMolecule = M <$> moleculeDecoder n
  | haveLabel lReaction = R <$> reactionDecoder n
  | otherwise =
    Left $ DatabaseError $
      DecodeError
        NodeDoesNotHaveExpectedLabel
          { expected = T.concat [lMolecule, " or ", lReaction]
          }
  where
    lMolecule = "Molecule"
    lReaction = "Reaction"
    haveLabel l = elem l $ labels n

pathDecoder :: Path -> Either Error [PathSegment]
pathDecoder (Path nodes rels _) = do
  vertexes <- mapM (fmap V . vertexDecoder) nodes
  edges <- mapM (fmap E . edgeDecoder) rels
  pure . concat $ transpose [vertexes, edges]
