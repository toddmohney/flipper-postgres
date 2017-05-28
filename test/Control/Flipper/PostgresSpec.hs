{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Flipper.PostgresSpec (main, spec) where

import           Control.Monad                   (void)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict                 as Map
import           Test.Hspec

import           Control.Flipper.Postgres        as FP
import           Control.Flipper.Postgres.Models (ConnectionPool)
import           Control.Flipper.Postgres.Query  as Q
import qualified Helpers.Config                  as Cfg

main :: IO ()
main = hspec spec

newtype MyContext m a = MyContext { unContext :: StateT MyState (FlipperT m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState MyState
             , MonadReader Config
             , HasFeatureFlags
             , ModifiesFeatureFlags
             )

newtype MyState = MyState Int
    deriving (Show, Eq)

runMyContext :: (MonadIO m)
             => ConnectionPool -> MyState -> MyContext m a -> m (a, MyState)
runMyContext pool initialState f =
    let flipperT = runStateT (unContext f) initialState
    in runFlipperT pool flipperT

spec :: Spec
spec = around Cfg.withConfig $ do
    describe "control flow with feature flags" $ do
        describe "a non-existant feature" $ do
            it "is disabled by default" $ \(Config pool _) -> do
                (_, st) <- runMyContext pool (MyState 0) $ do
                    whenEnabled "non-existant feature" (void $ put (MyState 1))

                st `shouldBe` MyState 0

        describe "a persisted feature" $ do
            it "runs features when it is enabled" $ \(Config pool dbAccess) -> do
                f <- Q.mkFeature (FP.FeatureName "enabled-feature") True
                void $ Q.addFeature f dbAccess

                (_, st) <- runMyContext pool (MyState 0) $ do
                    whenEnabled "enabled-feature" (void $ put (MyState 1))

                st `shouldBe` MyState 1

            it "does not run features it is are disabled" $ \(Config pool dbAccess) -> do
                f <- Q.mkFeature (FP.FeatureName "disabled-feature") False
                void $ Q.addFeature f dbAccess

                (_, st) <- runMyContext pool (MyState 0) $ do
                    whenEnabled "disabled-feature" (void $ put (MyState 1))

                st `shouldBe` MyState 0

    describe "modifying feature flags" $ do
        describe "new feature flags" $ do
            it "creates new records" $ \(Config pool dbAccess) -> do
                featureCount dbAccess `shouldReturn` 0

                (_, _) <- runMyContext pool (MyState 0) $ do
                    let fs = Features $ Map.fromList [ ("my-new-feature", True), ("some-other-feature", True) ]
                    updateFeatures fs

                featureCount dbAccess `shouldReturn` 2

            it "updates existing records" $ \(Config pool dbAccess) -> do
                featureCount dbAccess `shouldReturn` 0

                void $ runMyContext pool (MyState 0) $ do
                    let fs = Features $ Map.fromList [ ("my-new-feature", True), ("some-other-feature", True) ]
                    updateFeatures fs
                    liftIO $ featureCount dbAccess `shouldReturn` 2

                    let fs' = Features $ Map.fromList [ ("my-new-feature", False), ("some-other-feature", False), ("hi-there", False) ]
                    updateFeatures fs'
                    liftIO $ featureCount dbAccess `shouldReturn` 3

                    fs'' <- FP.getFeatures
                    liftIO $ all (== False) (Map.elems (unFeatures fs'')) `shouldBe` True


