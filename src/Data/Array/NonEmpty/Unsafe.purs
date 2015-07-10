module Data.Array.NonEmpty.Unsafe where

import Data.Array.NonEmpty (NonEmpty(), (:|))
import qualified Data.Array.Unsafe as U

fromArray :: forall a. Array a -> NonEmpty a
fromArray as = U.head as :| U.tail as
