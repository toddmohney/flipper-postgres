module Control.Flipper.Postgres.QuerySpec (main, spec) where

import           Control.Monad                            (void)
import           Test.Hspec

import           Control.Flipper.Adapters.Postgres
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
