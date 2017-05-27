{-# LANGUAGE RecordWildCards #-}

module Control.Flipper.Postgres.Query
    ( getFeatures
    , getFeatureByName
    , addFeature
    , replaceFeature
    , upsertFeature
    , featureCount
    , M.mkFeature
    ) where

import           Control.Monad                     (void)
import           Data.Time.Clock                   (getCurrentTime)

import           Control.Flipper.Postgres.DBAccess as DB
import           Control.Flipper.Postgres.Models as M
import qualified Control.Flipper.Types             as T
import           Control.Monad.IO.Class            (MonadIO, liftIO)

getFeatures :: (MonadIO app, Monad m)
            => DBAccess m -> app [Entity Feature]
getFeatures DBAccess{..} = liftIO $ runDb selectFeatures

getFeatureByName :: (MonadIO app, Monad m)
                 => T.FeatureName -> DBAccess m -> app (Maybe (Entity Feature))
getFeatureByName fName DBAccess{..} = liftIO $ runDb (findFeature fName)

upsertFeature :: (MonadIO app, Monad m)
              => T.FeatureName -> Bool -> DBAccess m -> app ()
upsertFeature fName isEnabled dbAccess = do
    mFeature <- getFeatureByName fName dbAccess
    case mFeature of
        Nothing ->
            liftIO (mkFeature fName isEnabled) >>= void . flip addFeature dbAccess
        (Just (Entity fId f)) ->
            replaceFeature fId (f { featureEnabled = isEnabled }) dbAccess

addFeature :: (MonadIO app, Monad m)
           => Feature -> DBAccess m -> app (Key Feature)
addFeature feature DBAccess{..} = liftIO $ runDb (insertFeature feature)

replaceFeature :: (MonadIO app, Monad m)
               => FeatureId -> Feature -> DBAccess m -> app ()
replaceFeature fId feature DBAccess{..} = do
    now <- liftIO getCurrentTime
    liftIO $ runDb (updateFeature fId (feature { featureUpdated = now }))

featureCount :: (MonadIO app, Monad m)
             => DBAccess m -> app Int
featureCount DBAccess{..} = liftIO $ runDb countFeatures
