{-# LANGUAGE RecordWildCards #-}

module Control.Flipper.Adapters.Postgres.Query
    ( getFeatures
    , getFeatureByName
    , addFeature
    , replaceFeature
    , upsertFeature
    , featureCount
    , M.mkFeature
    ) where

import           Control.Monad                              (void)
import           Data.Time.Clock                            (getCurrentTime)

import           Control.Flipper.Adapters.Postgres.DBAccess as DB
import           Control.Flipper.Adapters.Postgres.Models   as M
import qualified Control.Flipper.Types                      as T
import           Control.Monad.IO.Class                     (MonadIO, liftIO)

{- |
Selects all feature records
-}
getFeatures :: (MonadIO app, Monad m)
            => DBAccess m -> app [Entity Feature]
getFeatures DBAccess{..} = liftIO $ runDb selectFeatures

{- |
Selects a feature record by its unique name
-}
getFeatureByName :: (MonadIO app, Monad m)
                 => T.FeatureName -> DBAccess m -> app (Maybe (Entity Feature))
getFeatureByName fName DBAccess{..} = liftIO $ runDb (findFeature fName)

{- |
Inserts a new feature record if one with a matching name does not already exist.
Updates an existing feature record if one with a matching name already exists.
-}
upsertFeature :: (MonadIO app, Monad m)
              => T.FeatureName -> T.Feature -> DBAccess m -> app ()
upsertFeature fName feature dbAccess = do
    mFeature <- getFeatureByName fName dbAccess
    case mFeature of
        Nothing ->
            liftIO (featureToModel feature) >>= void . flip addFeature dbAccess
        (Just (Entity fId f)) ->
            replaceFeature fId (f { featureEnabled = T.isEnabled feature }) dbAccess

{- |
Inserts a new feature record.
-}
addFeature :: (MonadIO app, Monad m)
           => Feature -> DBAccess m -> app (Key Feature)
addFeature feature DBAccess{..} = liftIO $ runDb (insertFeature feature)

{- |
Updates an existing feature record.
-}
replaceFeature :: (MonadIO app, Monad m)
               => FeatureId -> Feature -> DBAccess m -> app ()
replaceFeature fId feature DBAccess{..} = do
    now <- liftIO getCurrentTime
    liftIO $ runDb (updateFeature fId (feature { featureUpdated = now }))

{- |
Returns a count of all feature records
-}
featureCount :: (MonadIO app, Monad m)
             => DBAccess m -> app Int
featureCount DBAccess{..} = liftIO $ runDb countFeatures
