module Biocad.App (BiocadM (..), Env (..)) where

import Biocad.Database (MonadDB (..), Query (..))
import Biocad.Error (Error (..), fromBoltError)
import Control.Lens ((^.))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Either.Combinators (mapLeft)
import Database.Bolt (Pipe, at, query, queryP, runE)
import GHC.Generics (Generic)
import Control.Monad.Error.Class (liftEither)

data Env
  = Env
      { connection :: Pipe
      }
  deriving (Generic)

newtype BiocadM a
  = BiocadM
      { runBiocadM ::  ReaderT Env (ExceptT Error IO) a
      }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError Error)


instance MonadDB BiocadM where
  runQuery = helper
    where
      helper (QueryP (var, q, params)) = BiocadM . runAction var $ queryP q params
      helper (Query (var, q)) = BiocadM . runAction var $ query q
      runAction v f = do
        env <- ask
        let pipe = env ^. #connection
        records <- runE pipe $ do
          r <- f
          mapM (`at` v) r
        liftEither $ mapLeft (fromBoltError) records
