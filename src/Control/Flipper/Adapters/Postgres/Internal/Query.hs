module Control.Flipper.Adapters.Postgres.Internal.Query
    ( selectFeatures
    , selectActorsByFeatureId
    , findFeature
    , insertActor
    , deleteActor
    , insertFeature
    , updateFeature
    , countActors
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
Selects all actors for a given feature records
-}
selectActorsByFeatureId :: FeatureId -> SqlPersistT IO [Entity Actor]
selectActorsByFeatureId fId = selectList [ActorFeatureId ==. fId] []

{- |
Selects a feature record by its unique name
-}
findFeature :: T.FeatureName -> SqlPersistT IO (Maybe (Entity Feature))
findFeature fName = getBy (UniqueFeatureName fName)

{- |
Inserts a new actor record.
-}
insertActor :: Actor -> SqlPersistT IO (Key Actor)
insertActor = insert

{- |
Deletes an actor record.
-}
deleteActor :: FeatureId -> T.ActorId -> SqlPersistT IO ()
deleteActor fId aId = deleteBy (UniqueActorIdFeatureId aId fId)

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

{- |
Returns a count of all feature records
-}
countActors :: SqlPersistT IO Int
countActors = count ([] :: [Filter Actor])
