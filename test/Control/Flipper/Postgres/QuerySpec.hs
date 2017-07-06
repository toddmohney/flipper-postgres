module Control.Flipper.Postgres.QuerySpec (main, spec) where

import           Control.Monad                            (void)
import qualified Data.Set as S
import           Test.Hspec

import           Control.Flipper.Adapters.Postgres
import qualified Control.Flipper.Adapters.Postgres.Query  as Q
import qualified Control.Flipper.Types                    as T
import qualified Helpers.Config                           as Cfg

main :: IO ()
main = hspec spec

spec :: Spec
spec = around Cfg.withConfig $ do
    describe "addFeature" $ do
        it "creates a new Feature and all associated Actors" $ \(Config _ db) -> do
            let actors = S.fromList [ActorId "thing:123", ActorId "blah:456", ActorId "nah:789"]
            let feature = (T.mkFeature "my-feature") { enabledActors = actors }
            void $ Q.upsertFeature feature db
            Q.featureCount db `shouldReturn` 1
            Q.actorCount db `shouldReturn` 3

        it "handles duplicate actors" $ \(Config _ db) -> do
            let actors = S.fromList [ActorId "thing:123", ActorId "blah:456", ActorId "nah:789"]
            let feature = (T.mkFeature "my-feature") { enabledActors = actors }
            void $ Q.upsertFeature feature db


            let actors' = S.fromList [ActorId "blah:456", ActorId "nah:789", ActorId "ack:000", ActorId "ack:001"]
            let feature' = (T.mkFeature "my-feature") { enabledActors = actors' }
            void $ Q.upsertFeature feature' db
            Q.featureCount db `shouldReturn` 1
            Q.actorCount db `shouldReturn` 4

    describe "upsertFeature" $ do
        it "creates a new feature when no feature by the given name exists" $ \(Config _ db) -> do
            let name = (T.FeatureName "experimental-feature")
            let feature = (T.mkFeature name) { isEnabled = True }
            Q.upsertFeature feature db
            (Just f) <- Q.getFeatureByName name db
            T.featureName f `shouldBe` name
            T.isEnabled f `shouldBe` True

        it "updates an existing feature when a feature by the given name exists" $ \(Config _ db) -> do
            let name = (T.FeatureName "experimental-feature")
            let f = (T.mkFeature name) { isEnabled = True }
            void $ Q.addFeature f db
            Q.featureCount db `shouldReturn` 1

            let f' = f { isEnabled = False }
            Q.upsertFeature f' db
            (Just feature) <- Q.getFeatureByName name db
            T.isEnabled feature `shouldBe` False
