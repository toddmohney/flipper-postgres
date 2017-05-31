module Control.Flipper.Postgres.QuerySpec (main, spec) where

import           Control.Monad                            (void)
import           Test.Hspec

import           Control.Flipper.Adapters.Postgres
import           Control.Flipper.Adapters.Postgres.Models as M
import qualified Control.Flipper.Adapters.Postgres.Query  as Q
import qualified Control.Flipper.Types                    as T
import qualified Helpers.Config                           as Cfg

main :: IO ()
main = hspec spec

spec :: Spec
spec = around Cfg.withConfig $ do
    describe "upsertFeature" $ do
        it "creates a new feature when no feature by the given name exists" $ \(Config _ db) -> do
            let name = (T.FeatureName "experimental-feature")
            Q.upsertFeature name True db
            (Just (Entity _ feature)) <- Q.getFeatureByName name db
            featureName feature `shouldBe` name
            featureEnabled feature `shouldBe` True

        it "updates an existing feature when a feature by the given name exists" $ \(Config _ db) -> do
            let name = (T.FeatureName "experimental-feature")
            f <- M.mkFeature name True
            void $ Q.addFeature f db
            Q.featureCount db `shouldReturn` 1

            Q.upsertFeature name False db
            (Just (Entity _ feature)) <- Q.getFeatureByName name db
            featureEnabled feature `shouldBe` False
