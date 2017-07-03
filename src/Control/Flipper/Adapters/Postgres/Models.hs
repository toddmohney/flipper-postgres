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

import qualified Data.ByteString.Char8       as C8
import qualified Data.Map.Strict                            as Map
import           Data.Monoid                 ((<>))
import qualified Data.Set as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
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
  toPersistValue = PersistDbSpecific . T.encodeUtf8 . F.unFeatureName
  fromPersistValue (PersistText name) = Right (F.FeatureName name)
  fromPersistValue name = Left ("Not PersistText " <> T.pack (show name))

instance PersistField F.ActorId where
  toPersistValue (F.ActorId actorId) = PersistByteString actorId
  fromPersistValue (PersistByteString actorId) = Right (F.ActorId actorId)
  fromPersistValue e = Left ("Not PersistByteString " <> T.pack (show e))

instance PersistField F.Percentage where
  toPersistValue (F.Percentage pct) = PersistDbSpecific . C8.pack $ show pct
  fromPersistValue (PersistInt64 pct) = Right (F.Percentage (fromIntegral pct))
  fromPersistValue e = Left ("Not PersistDbSpecific " <> T.pack (show e))

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
    , F.enabledEntities = S.empty
    , F.enabledPercentage = featureEnabledPercentage feature
    }

featureToModel :: F.Feature -> IO Feature
featureToModel f = do
    now <- getCurrentTime
    return Feature
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
