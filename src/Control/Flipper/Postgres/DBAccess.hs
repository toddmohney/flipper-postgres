{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Control.Flipper.Postgres.DBAccess
    ( DBAccess(..)
    , db
    ) where

import           Control.Monad.IO.Class                  (liftIO)

import qualified Control.Flipper.Postgres.Internal.Query as Q
import           Control.Flipper.Postgres.Models
import qualified Control.Flipper.Types                   as T

data DBAccess m = DBAccess { runDb          :: forall a . m a -> IO a
                           , selectFeatures :: m [Entity Feature]
                           , findFeature    :: T.FeatureName -> m (Maybe (Entity Feature))
                           , insertFeature  :: Feature -> m (Key Feature)
                           , updateFeature  :: FeatureId -> Feature -> m ()
                           , countFeatures  :: m Int
                           }

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

