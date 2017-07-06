{-# LANGUAGE RecordWildCards #-}

module Control.Flipper.Adapters.Postgres.Query
    ( getFeatures
    , getFeatureByName
    , addFeature
    , upsertFeature
    , actorCount
    , featureCount
    , M.mkFeature
    ) where

import           Control.Monad                              (forM_, void)
import           Data.Set                                   (Set)
import qualified Data.Set                                   as S
import           Data.Time.Clock                            (getCurrentTime)

import           Control.Flipper.Adapters.Postgres.DBAccess as DB
import           Control.Flipper.Adapters.Postgres.Models   as M
import qualified Control.Flipper.Types                      as T
import           Control.Monad.IO.Class                     (MonadIO, liftIO)

{- |
Selects all feature records
Returns domain model
-}
getFeatures :: (MonadIO app, Monad m)
            => DBAccess m -> app T.Features
getFeatures dbAccess = modelsToFeatures <$> getFeatures' dbAccess

{- |
Selects all feature records
Returns database entities
-}
getFeatures' :: (MonadIO app, Monad m)
            => DBAccess m -> app [Entity Feature]
getFeatures' DBAccess{..} = liftIO $ runDb selectFeatures

{- |
Selects a feature record by its unique name
Returns a domain model
-}
getFeatureByName :: (MonadIO app, Monad m)
                 => T.FeatureName -> DBAccess m -> app (Maybe T.Feature)
getFeatureByName fName dbAccess = do
    mFeatureEnt <- getFeatureByName' fName dbAccess
    case mFeatureEnt of
        Nothing                   -> return Nothing
        (Just (Entity _ feature)) -> return . Just . modelToFeature $ feature

{- |
Selects a feature record by its unique name
Returns a database entity
-}
getFeatureByName' :: (MonadIO app, Monad m)
                 => T.FeatureName -> DBAccess m -> app (Maybe (Entity Feature))
getFeatureByName' fName DBAccess{..} = liftIO $ runDb (findFeature fName)

{- |
Inserts a new feature record if one with a matching name does not already exist.
Updates an existing feature record if one with a matching name already exists.
-}
upsertFeature :: (MonadIO app, Monad m)
              => T.Feature -> DBAccess m -> app ()
upsertFeature feature dbAccess = do
    mFeature <- getFeatureByName' (T.featureName feature) dbAccess
    case mFeature of
        Nothing ->
            liftIO (featureToModel feature) >>= void . flip addFeature' dbAccess
        (Just (Entity fId _)) -> do
            updatedFeature <- liftIO $ featureToModel feature
            replaceFeature fId updatedFeature dbAccess

{- |
Inserts a new feature record and all associated actors.
-}
addFeature :: (MonadIO app, Monad m)
           => T.Feature -> DBAccess m -> app (Key Feature)
addFeature feature dbAccess = do
    model <- liftIO $ featureToModel feature
    addFeature' model dbAccess

{- |
Inserts a new feature record and all associated actors.
-}
addFeature' :: (MonadIO app, Monad m)
           => FeatureWithActorIds -> DBAccess m -> app (Key Feature)
addFeature' (feature, actorIds) dbAccess@DBAccess{..} = do
    key <- liftIO $ runDb (insertFeature feature)
    addActors actorIds key dbAccess
    return key

addActors :: (MonadIO app, Monad m)
          => Set T.ActorId -> FeatureId -> DBAccess m -> app ()
addActors actorIds fId DBAccess{..} =
    liftIO $ forM_ actorIds $ \aId ->
        actorIdToModel aId fId >>= runDb . insertActor

deleteActors :: (MonadIO app, Monad m)
             => Set T.ActorId -> FeatureId -> DBAccess m -> app ()
deleteActors actorIds fId DBAccess{..} =
    liftIO $ forM_ actorIds $ runDb . deleteActor fId

{- |
Updates an existing feature record.
-}
replaceFeature :: (MonadIO app, Monad m)
               => FeatureId -> FeatureWithActorIds -> DBAccess m -> app ()
replaceFeature fId (feature, newActorIds) dbAccess@DBAccess{..} = do
    now <- liftIO getCurrentTime
    oldActorIds <- (S.fromList . map (actorActorId . entityVal)) <$> liftIO (runDb (selectActorsByFeatureId fId))

    let (toAdd, toDelete) = actorDiff oldActorIds newActorIds
    addActors toAdd fId dbAccess
    deleteActors toDelete fId dbAccess

    liftIO $ runDb (updateFeature fId (feature { featureUpdated = now }))

actorDiff :: Set T.ActorId -> Set T.ActorId -> (Set T.ActorId, Set T.ActorId)
actorDiff oldActorIds newActorIds =
    let actorIdsToAdd = S.difference newActorIds oldActorIds
        actorIdsToDelete = S.difference oldActorIds newActorIds
    in (actorIdsToAdd, actorIdsToDelete)

{- |
Returns a count of all feature records
-}
featureCount :: (MonadIO app, Monad m)
             => DBAccess m -> app Int
featureCount DBAccess{..} = liftIO $ runDb countFeatures

{- |
Returns a count of all actor records
-}
actorCount :: (MonadIO app, Monad m)
             => DBAccess m -> app Int
actorCount DBAccess{..} = liftIO $ runDb countActors
