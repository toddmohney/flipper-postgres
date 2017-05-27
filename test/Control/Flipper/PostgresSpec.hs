{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Flipper.PostgresSpec (main, spec) where

import           Control.Monad (void)
import           Control.Monad.Reader
import           Control.Monad.State
import           Test.Hspec

import           Control.Flipper.Postgres as FP
import           Control.Flipper.Postgres.Models (ConnectionPool)
import           Control.Flipper.Postgres.Query as Q
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
    describe "a non-existant feature" $ do
        it "is disabled by default" $ \(Config pool _) -> do
            (_, st) <- runMyContext pool (MyState 0) $ do
                whenEnabled "non-existant feature" (void $ put (MyState 1))

            st `shouldBe` MyState 0

    describe "a persisted feature" $ do
        it "runs features when it is enabled" $ \(Config pool dbAccess) -> do
            f <- Q.mkFeature (FP.FeatureName "some-feature") True
            void $ Q.addFeature f dbAccess

            (_, st) <- runMyContext pool (MyState 0) $ do
                whenEnabled "some-feature" (void $ put (MyState 1))

            st `shouldBe` MyState 1

        it "does not run features it is are disabled" $ \(Config pool dbAccess) -> do
            f <- Q.mkFeature (FP.FeatureName "some-feature") False
            void $ Q.addFeature f dbAccess

            (_, st) <- runMyContext pool (MyState 0) $ do
                whenEnabled "some-feature" (void $ put (MyState 1))

            st `shouldBe` MyState 0
