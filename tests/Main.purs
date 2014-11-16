module Main where

import Test.QuickCheck
import qualified Data.Array as A
import Data.Array.NonEmpty

foreign import undefined :: forall a. a

instance arbNonEmpty :: (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = undefined

main = do
    quickCheck \xs -> length xs == A.length (toArray xs)