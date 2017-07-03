{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Flipper.PostgresSpec (main, spec) where

import           Control.Monad                            (void)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict                          as Map
import           Test.Hspec

import           Control.Flipper.Adapters.Postgres        as FP
import           Control.Flipper.Adapters.Postgres.Models (ConnectionPool)
import           Control.Flipper.Adapters.Postgres.Query  as Q
import qualified Helpers.Config                           as Cfg

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
                    let feature1 = (FP.mkFeature "my-new-feature") { isEnabled = True }
                    let feature2 = (FP.mkFeature "some-other-feature") { isEnabled = True }
                    let fs = Features $ Map.fromList [ (featureName feature1, feature1), (featureName feature2, feature2) ]
                    updateFeatures fs

                featureCount dbAccess `shouldReturn` 2

            it "updates existing records" $ \(Config pool dbAccess) -> do
                featureCount dbAccess `shouldReturn` 0

                void $ runMyContext pool (MyState 0) $ do
                    let feature1 = (FP.mkFeature "my-new-feature") { isEnabled = True }
                    let feature2 = (FP.mkFeature "some-other-feature") { isEnabled = True }
                    let featureList = [ (featureName feature1, feature1), (featureName feature2, feature2) ]
                    let fs = Features $ Map.fromList featureList
                    updateFeatures fs
                    liftIO $ featureCount dbAccess `shouldReturn` 2

                    let feature1' = (FP.mkFeature "my-new-feature") { isEnabled = False }
                    let feature2' = (FP.mkFeature "some-other-feature") { isEnabled = False }
                    let feature3' = (FP.mkFeature "hi-there") { isEnabled = False }
                    let featureList' = [ (featureName feature1', feature1'), (featureName feature2', feature2'), (featureName feature3', feature3') ]
                    let fs' = Features $ Map.fromList featureList'
                    updateFeatures fs'
                    liftIO $ featureCount dbAccess `shouldReturn` 3

                    fs'' <- FP.getFeatures
                    liftIO $ all (\f -> isEnabled f == False) (Map.elems (unFeatures fs'')) `shouldBe` True


