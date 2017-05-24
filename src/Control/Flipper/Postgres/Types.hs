{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Flipper.Postgres.Types
    ( Config(..)
    , FlipperT(..)
    ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Map.Strict as Map
import           Database.Persist.Postgresql (ConnectionPool)

import           Control.Flipper.Postgres.DBAccess
import           Control.Flipper.Postgres.Models
import qualified Control.Flipper.Postgres.Query as Q
import           Control.Flipper.Types       (Features(..),
                                              FeatureName,
                                              HasFeatureFlags (..),
                                              ModifiesFeatureFlags (..))

newtype FlipperT a = FlipperT { unFlipper :: ReaderT Config (LoggingT IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadReader Config
             )

data Config = forall m. (Monad m) => Config
    { appDBConn    :: ConnectionPool
    , appDB        :: DBAccess m
    }

instance HasFeatureFlags FlipperT where
    getFeatures = ask >>= \Config{..} ->
        modelsToFeatures <$> Q.getFeatures appDB

    getFeature name = ask >>= \Config{..} -> do
        mFeature <- Q.getFeatureByName name appDB
        case mFeature of
            Nothing -> return Nothing
            (Just (Entity _ f)) -> return $ Just (featureEnabled f)

instance ModifiesFeatureFlags FlipperT where
    updateFeatures = undefined

    updateFeature fName isEnabled = ask >>= \Config{..} ->
        Q.upsertFeature fName isEnabled appDB

modelsToFeatures :: [Entity Feature] -> Features
modelsToFeatures fs = Features $ Map.fromList $ map mkFeaturesTup fs

mkFeaturesTup :: Entity Feature -> (FeatureName, Bool)
mkFeaturesTup (Entity _ feature) = (featureName feature, featureEnabled feature)
