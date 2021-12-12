module Data.RAListSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Data.String.Strip
import Data.RAList

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

prop_size =
    True


spec :: Spec
spec = do
    describe "RAList" $ do
        it "is equal when apply from-from" $ property $ prop_size


