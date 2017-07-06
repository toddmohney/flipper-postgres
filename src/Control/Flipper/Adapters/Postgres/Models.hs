{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Control.Flipper.Adapters.Postgres.Models
    ( module Control.Flipper.Adapters.Postgres.Models
    , module Database.Persist.Postgresql
    ) where

import qualified Data.Map.Strict                            as Map
import           Data.Monoid                 ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime (..), getCurrentTime)
import           Database.Persist.Postgresql
import           Database.Persist.TH

import qualified Control.Flipper.Types       as F

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Actor sql=flipper_actors
        actorId F.ActorId sqltype=bytea
        featureId FeatureId sqltype=bigint
        updated UTCTime default=now()
        created UTCTime default=now()
        UniqueActorIdFeatureId actorId featureId
        deriving Show Eq

    Feature sql=flipper_features
        name F.FeatureName sqltype=text
        enabled Bool sqltype=boolean default=false
        enabledPercentage F.Percentage sqltype=int default=0
        updated UTCTime default=now()
        created UTCTime default=now()
        UniqueFeatureName name
        deriving Show Eq
|]

instance PersistField F.FeatureName where
  toPersistValue = PersistText . F.unFeatureName
  fromPersistValue (PersistText name) = Right (F.FeatureName name)
  fromPersistValue name = Left ("Not PersistText " <> T.pack (show name))

instance PersistField F.ActorId where
  toPersistValue (F.ActorId actorId) = PersistByteString actorId
  fromPersistValue (PersistByteString actorId) = Right (F.ActorId actorId)
  fromPersistValue e = Left ("Not PersistByteString " <> T.pack (show e))

instance PersistField F.Percentage where
  toPersistValue (F.Percentage pct) = PersistInt64 (fromIntegral pct)
  fromPersistValue (PersistInt64 pct) = Right (F.Percentage (fromIntegral pct))
  fromPersistValue e = Left ("Not PersistInt64 " <> T.pack (show e))

type FeatureWithActorIds = (Feature, Set F.ActorId)

{- |
Convienience constructor
-}
mkFeature :: F.FeatureName -> Bool -> IO Feature
mkFeature fName isEnabled = do
    now <- getCurrentTime
    return Feature
        { featureName = fName
        , featureEnabled = isEnabled
        , featureEnabledPercentage = 0
        , featureUpdated = now
        , featureCreated = now
        }

modelsToFeatures :: [Entity Feature] -> F.Features
modelsToFeatures fs = F.Features $ Map.fromList $ map (toFeatureTuple . modelToFeature . entityVal) fs

modelToFeature :: Feature -> F.Feature
modelToFeature feature = F.Feature
    { F.featureName = featureName feature
    , F.isEnabled = featureEnabled feature
    , F.enabledActors = S.empty
    , F.enabledPercentage = featureEnabledPercentage feature
    }

actorIdToModel :: F.ActorId -> FeatureId -> IO Actor
actorIdToModel a f = do
    now <- getCurrentTime
    return Actor
        { actorActorId = a
        , actorFeatureId = f
        , actorUpdated = now
        , actorCreated = now
        }

featureToModel :: F.Feature -> IO FeatureWithActorIds
featureToModel f = do
    now <- getCurrentTime
    return (feature now, actorIds)
    where
        actorIds :: Set F.ActorId
        actorIds = F.enabledActors f

        feature :: UTCTime -> Feature
        feature now = Feature
            { featureName = F.featureName f
            , featureEnabled = F.isEnabled f
            , featureEnabledPercentage = F.enabledPercentage f
            , featureUpdated = now
            , featureCreated = now
            }

toFeatureTuple :: F.Feature -> (F.FeatureName, F.Feature)
toFeatureTuple f = (F.featureName f, f)

{- |
Performs non-destructive database schema migrations.
-}
runMigrations :: ConnectionPool -> IO [Text]
runMigrations = runSqlPool (runMigrationSilent migrateAll)
