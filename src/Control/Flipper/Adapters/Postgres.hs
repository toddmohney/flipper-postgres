{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Control.Flipper.Adapters.Postgres
    ( Config(..)
    , FlipperT(..)
    , runFlipperT
    , module Control.Flipper
    ) where
import           Control.Monad.IO.Class                     (MonadIO)
import           Control.Monad.Reader
import           Control.Monad.Trans                        (MonadTrans)
import qualified Data.Map.Strict                            as Map
import           Database.Persist.Postgresql                (ConnectionPool)

import           Control.Flipper.Adapters.Postgres.DBAccess (DBAccess, db)
import           Control.Flipper.Adapters.Postgres.Models
import qualified Control.Flipper.Adapters.Postgres.Query    as Q
import           Control.Flipper.Types                      (FeatureName,
                                                             Features (..),
                                                             HasFeatureFlags (..),
                                                             ModifiesFeatureFlags (..))

import           Control.Flipper

newtype FlipperT m a = FlipperT { unFlipper :: ReaderT Config m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Config
             , MonadTrans
             )

runFlipperT :: (MonadIO m)
            => ConnectionPool -> FlipperT m a -> m a
runFlipperT pool f =
    let cfg = Config pool (db pool)
    in runReaderT (unFlipper f) cfg

instance (MonadIO m) => HasFeatureFlags (FlipperT m) where
    getFeatures = ask >>= \Config{..} ->
        modelsToFeatures <$> Q.getFeatures appDB

    getFeature name = ask >>= \Config{..} -> do
        mFeature <- Q.getFeatureByName name appDB
        case mFeature of
            Nothing             -> return Nothing
            (Just (Entity _ f)) -> return $ Just (featureEnabled f)

instance (MonadIO m) => ModifiesFeatureFlags (FlipperT m) where
    updateFeatures features =
        void $ Map.traverseWithKey updateFeature (unFeatures features)

    updateFeature fName isEnabled = ask >>= \Config{..} ->
        Q.upsertFeature fName isEnabled appDB

modelsToFeatures :: [Entity Feature] -> Features
modelsToFeatures fs = Features $ Map.fromList $ map mkFeature' fs
    where
        mkFeature' :: Entity Feature -> (FeatureName, Bool)
        mkFeature' (Entity _ feature) =
            (featureName feature, featureEnabled feature)

data Config = forall m. (Monad m) => Config
    { appDBConn :: ConnectionPool
    , appDB     :: DBAccess m
    }
