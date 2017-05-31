module Control.Flipper.Adapters.Postgres.Internal.Query
    ( selectFeatures
    , findFeature
    , insertFeature
    , updateFeature
    , countFeatures
    ) where

import           Control.Flipper.Adapters.Postgres.Models as M
import qualified Control.Flipper.Types                    as T

selectFeatures :: SqlPersistT IO [Entity Feature]
selectFeatures = selectList [] []

findFeature :: T.FeatureName -> SqlPersistT IO (Maybe (Entity Feature))
findFeature fName = getBy (UniqueFeatureName fName)

insertFeature :: Feature -> SqlPersistT IO (Key Feature)
insertFeature = insert

updateFeature :: M.FeatureId -> M.Feature -> SqlPersistT IO ()
updateFeature = replace

countFeatures :: SqlPersistT IO Int
countFeatures = count ([] :: [Filter Feature])
