module Main where

import Test.QuickCheck
import qualified Data.Array as A
import Data.Array.NonEmpty
import Debug.Trace

foreign import undefined :: forall a. a

instance arbNonEmpty :: (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = undefined

main = do
    Debug.Trace.trace "first test"
    quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))

    quickCheck $ \a b c -> ((a :: Boolean && b) || c) == ((a || c) && (b || c))