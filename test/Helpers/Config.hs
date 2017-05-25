module Helpers.Config
    ( withConfig
    ) where

import qualified Control.Flipper.Postgres.DBAccess as DBA
import           Control.Flipper.Postgres.Types
import qualified Helpers.Database                  as DB

withConfig :: (Config -> IO ()) -> IO ()
withConfig action = do
    cfg@(Config conn _) <- loadConfig
    DB.setupTestDatabase conn
    action cfg

loadConfig :: IO Config
loadConfig = do
    conn <- DB.makePool
    return Config
        { appDBConn = conn
        , appDB = DBA.db conn
        }
