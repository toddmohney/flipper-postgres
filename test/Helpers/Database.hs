module Helpers.Database
    ( makePool
    , setupTestDatabase
    ) where

import           Control.Monad.Logger                     (runNoLoggingT)
import           Data.ByteString.Char8                    (pack)
import           Database.Persist.Postgresql              (ConnectionPool,
                                                           ConnectionString,
                                                           createPostgresqlPool,
                                                           rawExecute,
                                                           runSqlPool)
import           System.Environment                       (getEnv, lookupEnv)

import           Control.Flipper.Adapters.Postgres.Models (runMigrations)

data DbConnectionString =
  DbConnectionString { dbname   :: String
                     , user     :: Maybe String
                     , password :: Maybe String
                     , host     :: Maybe String
                     , port     :: Maybe Int
                     } deriving (Show)

makePool :: IO ConnectionPool
makePool = do
  connStr <- dbConnectionString
  poolSize <- read <$> getEnv "DB_POOL_SIZE"
  runNoLoggingT $ createPostgresqlPool connStr poolSize

dbConnectionString :: IO ConnectionString
dbConnectionString =
  (pack . toStr) <$> buildDBConnectionFromEnv

toStr :: DbConnectionString -> String
toStr cStr =
  unwords
    [ "dbname=" ++ dbname cStr
    , maybe ""               (\x -> "user=" ++ x)      (user cStr)
    , maybe ""               (\x -> "password=" ++ x)  (password cStr)
    , maybe "host=localhost" (\x -> "host=" ++ x)      (host cStr)
    , maybe "port=5432"      (\x -> "port=" ++ show x) (port cStr)
    ]

buildDBConnectionFromEnv :: IO DbConnectionString
buildDBConnectionFromEnv =
  DbConnectionString
  <$> getEnv    "DB_NAME"
  <*> lookupEnv "DB_USER"
  <*> lookupEnv "DB_PASS"
  <*> lookupEnv "DB_HOST"
  <*> (parsePort <$> lookupEnv "DB_PORT")

parsePort :: Maybe String -> Maybe Int
parsePort Nothing  = Nothing
parsePort (Just p) = Just (read p)

setupTestDatabase :: ConnectionPool -> IO ()
setupTestDatabase pool = do
    _ <- runMigrations pool
    truncateDatabase pool

truncateDatabase :: ConnectionPool -> IO ()
truncateDatabase pool = do
    runSqlPool truncateFeaturesQuery pool
    where
        truncateFeaturesQuery =
            rawExecute "TRUNCATE TABLE feature_flipper_features RESTART IDENTITY;" []
