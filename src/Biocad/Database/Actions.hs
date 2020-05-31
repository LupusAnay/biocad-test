module Biocad.Database.Actions
  ( createReaction,
    getReactionById,
    getShortestPath,
    getMoleculeById,
    getShortestPathByMoleculeId,
  )
where

import Biocad.Data (Id, Molecule, PathSegment, Reaction)
import Biocad.Database.Decoders
import Biocad.Database.Effects
import Biocad.Database.Queries
import Biocad.Error (DatabaseError (..), Error (..))
import Control.Lens (firstOf)
import Control.Monad.Except (MonadError, liftEither)
import Data.Either.Combinators (maybeToRight)

createReaction :: (MonadDB m, MonadError Error m) => Reaction -> m ()
createReaction reaction = do
  eitherNodes <- runQuery (createReactionQuery reaction)
  let _ = mapM reactionDecoder eitherNodes
  pure ()

getReactionById :: (MonadDB m, MonadError Error m) => Id -> m Reaction
getReactionById reactionId = do
  eitherNodes <- runQuery $ getReactionByIdQuery reactionId
  reactions <- mapM (liftEither . reactionDecoder) eitherNodes
  liftEither $ (maybeToRight (DatabaseError EntityNotFound)) . firstOf traverse $ reactions

getMoleculeById :: (MonadDB m, MonadError Error m) => Id -> m Molecule
getMoleculeById moleculeId = do
  eitherNodes <- runQuery $ getMoleculeByIdQuery moleculeId
  let molecule = mapM moleculeDecoder eitherNodes >>= (maybeToRight (DatabaseError EntityNotFound)) . firstOf traverse
  liftEither molecule

getShortestPath :: (MonadDB m, MonadError Error m) => Molecule -> Molecule -> m [PathSegment]
getShortestPath m1 m2 = do
  eitherPath <- runQuery $ getShortestPathQuery m1 m2
  let path = mapM pathDecoder eitherPath >>= (maybeToRight (DatabaseError EntityNotFound)) . firstOf traverse
  liftEither path

getShortestPathByMoleculeId :: (MonadDB m, MonadError Error m) => Id -> Id -> m [PathSegment]
getShortestPathByMoleculeId id1 id2 = do
  m1 <- getMoleculeById id1
  m2 <- getMoleculeById id2
  getShortestPath m1 m2
