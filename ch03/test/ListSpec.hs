module ListSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = do
    describe "listHead" $ do
        it "head" $ head ["foo", "foobar"] `shouldBe` "foo"
        it "head1" $ head ["bar", "foobar"] `shouldBe` "bar"
