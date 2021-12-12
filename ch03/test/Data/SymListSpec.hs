module Data.SymListSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Data.String.Strip
import Data.SymList

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

prop_from_from xs = 
    fromSL (fromList xs) == xs
    where types = xs::[Int]

prop_RevRev xs = reverse (reverse xs) == xs
    where types = xs::[Int]

spec :: Spec
spec = do
    describe "SymList" $ do
        it "is equal when apply from-from" $ property $ prop_from_from
        it "is equal to a list" $ property $ prop_RevRev
