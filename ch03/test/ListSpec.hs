module ListSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Lib

-- | Resource: https://github.com/commercialhaskell/rio/blob/master/rio/test/RIO/DequeSpec.hs

spec :: Spec
spec = do
    describe "listHead" $ do
        it "head" $ head ["foo", "foobar"] `shouldBe` "foo"
        it "head1" $ head ["bar", "foobar"] `shouldBe` "bar"
    describe "read" $ do
        prop "is inverse to show" $
            \x -> (read . show) x `shouldBe` (x :: Int)
