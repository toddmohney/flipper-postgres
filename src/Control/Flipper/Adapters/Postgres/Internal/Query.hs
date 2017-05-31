module Control.Flipper.Adapters.Postgres.Internal.Query
    ( selectFeatures
    , findFeature
    , insertFeature
    , updateFeature
    , countFeatures
    ) where

import           Control.Flipper.Adapters.Postgres.Models as M
import qualified Control.Flipper.Types                    as T

{- |
Selects all feature records
-}
selectFeatures :: SqlPersistT IO [Entity Feature]
selectFeatures = selectList [] []

{- |
Selects a feature record by its unique name
-}
findFeature :: T.FeatureName -> SqlPersistT IO (Maybe (Entity Feature))
findFeature fName = getBy (UniqueFeatureName fName)

{- |
Inserts a new feature record.
-}
insertFeature :: Feature -> SqlPersistT IO (Key Feature)
insertFeature = insert

{- |
Updates an existing feature record.
-}
updateFeature :: M.FeatureId -> M.Feature -> SqlPersistT IO ()
updateFeature = replace

{- |
Returns a count of all feature records
-}
countFeatures :: SqlPersistT IO Int
countFeatures = count ([] :: [Filter Feature])
