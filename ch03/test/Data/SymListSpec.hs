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

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

prop_from_from xs = 
    fromSL (fromList xs) == xs
    where types = xs::[Int]

prop_null xs =
    let xs' = fromList xs in null xs == nullSL xs'
    where types = xs::[Int]

prop_single xs =
    case xs of
        [_] -> let xs' = fromList xs in singleSL xs'
        xs -> let xs' = fromList xs in not $ singleSL xs'
    where types = xs::[Int]

prop_cons x xs =
    let xs' = fromList xs in x : fromSL xs' == fromSL (consSL x xs')
    where types = xs::[Int]

prop_snoc x xs =
    let xs' = fromList xs in fromSL xs' ++ [x] == fromSL (snocSL x xs')
    where types = xs::[Int]

prop_head xs =
    case xs of
        [] -> let xs' = fromList [] in isNothing (headSL xs')
        xs -> let xs' = fromList xs in Just (head (fromSL xs')) == headSL xs';
    where types = xs::[Int]

prop_last xs =
    case xs of
        [] -> let xs' = fromList [] in isNothing (lastSL xs')
        xs -> let xs' = fromList xs in Just (last (fromSL xs')) == lastSL xs';
    where types = xs::[Int]

prop_tail xs =
    case xs of
        [] -> let xs' = fromList [] in nullSL (tailSL xs')
        xs -> let xs' = fromList xs in fromSL (tailSL xs') == tail xs
    where types = xs::[Int]

prop_init xs =
    case xs of
        [] -> let xs' = fromList [] in nullSL (initSL xs')
        xs -> let xs' = fromList xs in fromSL (initSL xs') == init xs
    where types = xs::[Int]

spec :: Spec
spec = do
    describe "SymList" $ do
        it "is equal when apply from-from" $ property $ prop_from_from
        it "holds the cons invariant" $ property $ prop_cons
        it "holds the null invariant" $ property $ prop_null
        it "holds the single invariant" $ property $ prop_single
        it "holds the snoc invariant" $ property $ prop_snoc
        it "holds the head invariant" $ property $ prop_head
        it "holds the last invariant" $ property $ prop_last
        it "holds the tail invariant" $ property $ prop_tail
        it "holds the init invariant" $ property $ prop_init
