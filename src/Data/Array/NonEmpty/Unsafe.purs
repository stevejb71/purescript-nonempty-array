module Data.Array.NonEmpty.Unsafe where

import Data.Array.NonEmpty (NonEmpty(), (:|))

fromArray :: forall a. [a] -> NonEmpty a
fromArray (a:as) = a :| as