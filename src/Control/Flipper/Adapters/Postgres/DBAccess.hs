{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Control.Flipper.Adapters.Postgres.DBAccess
    ( DBAccess(..)
    , db
    ) where

import           Control.Monad.IO.Class                           (liftIO)

import qualified Control.Flipper.Adapters.Postgres.Internal.Query as Q
import           Control.Flipper.Adapters.Postgres.Models
import qualified Control.Flipper.Types                            as T

{- |
Database query interface
-}
data DBAccess m = DBAccess { runDb          :: forall a . m a -> IO a
                           , selectFeatures :: m [Entity Feature]
                           , findFeature    :: T.FeatureName -> m (Maybe (Entity Feature))
                           , insertFeature  :: Feature -> m (Key Feature)
                           , updateFeature  :: FeatureId -> Feature -> m ()
                           , countFeatures  :: m Int
                           }

{- |
Creates a DBAccess backed by a SqlPersistT context
-}
db :: ConnectionPool -> DBAccess (SqlPersistT IO)
db pool = DBAccess { runDb = runDb' pool
                   , selectFeatures = Q.selectFeatures
                   , findFeature    = Q.findFeature
                   , insertFeature  = Q.insertFeature
                   , updateFeature  = Q.updateFeature
                   , countFeatures  = Q.countFeatures
                   }
  where
    runDb' :: ConnectionPool -> SqlPersistT IO a -> IO a
    runDb' conn query = liftIO (runSqlPool query conn)

