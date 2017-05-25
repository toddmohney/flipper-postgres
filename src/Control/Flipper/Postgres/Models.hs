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

module Control.Flipper.Postgres.Models
    ( module Control.Flipper.Postgres.Models
    , module Database.Persist.Postgresql
    ) where

import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time                   (UTCTime (..), getCurrentTime)
import           Database.Persist.Postgresql
import           Database.Persist.TH

import qualified Control.Flipper.Types       as F

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Feature sql=feature_flipper_features
        name F.FeatureName sqltype=text
        enabled Bool sqltype=boolean default=false
        updated UTCTime default=now()
        created UTCTime default=now()
        UniqueFeatureName name
        deriving Show Eq
|]

instance PersistField F.FeatureName where
  toPersistValue = PersistDbSpecific . T.encodeUtf8 . F.unFeatureName
  fromPersistValue (PersistText name) = Right (F.FeatureName name)
  fromPersistValue name = Left ("Not PersistText " <> T.pack (show name))

mkFeature :: F.FeatureName -> Bool -> IO Feature
mkFeature fName isEnabled = do
    now <- getCurrentTime
    return Feature
        { featureName = fName
        , featureEnabled = isEnabled
        , featureUpdated = now
        , featureCreated = now
        }

runMigrations :: ConnectionPool -> IO [Text]
runMigrations = runSqlPool (runMigrationSilent migrateAll)
