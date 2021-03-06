{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Flipper.PostgresSpec (main, spec) where

import           Control.Monad                            (void)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Char8 as C8
import           Data.Map.Strict                          as Map
import qualified Data.Set                                 as Set
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
            it "runs a feature when it is enabled" $ \(Config pool dbAccess) -> do
                let f = (FP.mkFeature "enabled-feature") { isEnabled = True }
                void $ Q.addFeature f dbAccess

                (_, st) <- runMyContext pool (MyState 0) $ do
                    whenEnabled "enabled-feature" (void $ put (MyState 1))

                st `shouldBe` MyState 1

            it "does not run disabled features" $ \(Config pool dbAccess) -> do
                let f = (FP.mkFeature "disabled-feature") { isEnabled = False }
                void $ Q.addFeature f dbAccess

                (_, st) <- runMyContext pool (MyState 0) $ do
                    whenEnabled "disabled-feature" (void $ put (MyState 1))

                st `shouldBe` MyState 0

    describe "modifying feature flags" $ do
        describe "adding new feature flags" $ do
            it "creates new records" $ \(Config pool dbAccess) -> do
                featureCount dbAccess `shouldReturn` 0

                (_, _) <- runMyContext pool (MyState 0) $ do
                    let feature1 = (FP.mkFeature "my-new-feature") { isEnabled = True }
                    let feature2 = (FP.mkFeature "some-other-feature") { isEnabled = True }
                    let fs = Features $ Map.fromList [ (featureName feature1, feature1), (featureName feature2, feature2) ]
                    updateFeatures fs

                featureCount dbAccess `shouldReturn` 2

            it "updating existing feature records" $ \(Config pool dbAccess) -> do
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

    describe "enabling a feature for a specific actor" $ do
        it "runs a feature for enabled users" $ \(Config pool _) -> do
            let actor1 = User 1
            let actor2 = User 2

            -- setup the features
            runFlipperT pool $ do
                -- here, we only enable the feature for actor1
                let feature = (FP.mkFeature "vrry-special-feature") { isEnabled = False, enabledActors = Set.singleton (actorId actor1) }
                let fs = Features $ Map.singleton (featureName feature) feature
                updateFeatures fs

            -- run some computation with feature flippers
            (_, st) <- runMyContext pool (MyState 0) $ do
                whenEnabledFor "vrry-special-feature" actor1 $
                    (void $ put (MyState 1))

                whenEnabledFor "vrry-special-feature" actor2 $
                    (void $ put (MyState 2))

            st `shouldBe` MyState 1

data User = User { userId :: Int }
    deriving (Show, Eq)

instance HasActorId User where
    actorId = ActorId . C8.pack . show . userId
