module Biocad.Database.Queries
  ( createReactionQuery,
    getReactionByIdQuery,
    getShortestPathQuery,
    getMoleculeByIdQuery,
  )
where

import Biocad.Data (Id, Molecule, Reaction)
import Biocad.Database.Utils (Query(..))
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Database.Bolt ((=:), props)

createReactionQuery :: Reaction -> Query
createReactionQuery reaction =
  QueryP
    ( "r",
      "CREATE (r:Reaction {id: {id}, name: {name}}) return r",
      props ["id" =: (reaction ^. #id), "name" =: (reaction ^. #name)]
    )

getReactionByIdQuery :: Id -> Query
getReactionByIdQuery reactionId =
  QueryP
    ("r", "MATCH (r:Reaction) WHERE r.id = {id} RETURN r", props ["id" =: reactionId])

getShortestPathQuery :: Molecule -> Molecule -> Query
getShortestPathQuery m1 m2 =
  QueryP
    ( "p",
      "MATCH (m1:Molecule), (m2:Molecule), p = shortestPath((m1)-[*]-(m2)) WHERE m1.id = {id1} AND m2.id = {id2} RETURN p",
      props ["id1" =: (m1 ^. #id), "id2" =: (m2 ^. #id)]
    )

getMoleculeByIdQuery :: Id -> Query
getMoleculeByIdQuery moleculeId =
  QueryP ("m", "MATCH (m:Molecule) WHERE m.id = {id} RETURN m", props ["id" =: moleculeId])
