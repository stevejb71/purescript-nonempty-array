module Data.Array.NonEmpty
  ( NonEmpty(..)
  , (:|)
  , (!!)
  , toArray
  , length
  , head
  , tail
  , last
  , push
  , pop
  , (<|)
  , take
  , drop
  , filter
  , singleton
  , nub
  , nubBy
  , concatMap
  , reverse
  , reducer
  , reducel
  , sconcat
  ) where

import Prelude
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Maybe (Maybe(..))
import qualified Data.Array as A
import qualified Data.Array.Unsafe as U
import qualified Data.Traversable as T

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
    show (NonEmpty a as) = "[" ++ (show a) ++ ":|" ++ (show as) ++ "]"

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty l ls) (NonEmpty r rs) = l == r && ls == rs

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a as) = f a :| (map f as)

instance applyNonEmpty :: Apply NonEmpty where
  apply x y = fromArray_ $ (toArray x) <*> (toArray y)

instance applicativeNonEmpty :: Applicative NonEmpty where
  pure = singleton

instance bindNonEmpty :: Bind NonEmpty where
  bind = flip concatMap

instance monadNonEmpty :: Monad NonEmpty

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a as) ys = a :| append as (toArray ys)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f b as = foldr f b (toArray as)
  foldl f b as = foldl f b (toArray as)
  foldMap f as = foldMap f (toArray as)

instance traversableNonEmpty :: T.Traversable NonEmpty where
  traverse = traverse_
  sequence = traverse_ id

traverse_ :: forall a b m. (Applicative m) => (a -> m b) -> NonEmpty a -> m (NonEmpty b)
traverse_ f (NonEmpty a []) = singleton <$> f a
traverse_ f (NonEmpty a as) = (<|) <$> (f a) <*> traverse_ f (fromArray_ as)

infix 5 :|
(:|) :: forall a. a -> Array a -> NonEmpty a
(:|) a as = NonEmpty a as

toArray :: forall a. NonEmpty a -> Array a
toArray (NonEmpty a as) = a `A.cons` as

length :: forall a. NonEmpty a -> Int
length (NonEmpty _ as) = 1 + A.length as

head :: forall a. NonEmpty a -> a
head (NonEmpty a _) = a

tail :: forall a. NonEmpty a -> Array a
tail (NonEmpty _ as) = as

last :: forall a. NonEmpty a -> a
last (NonEmpty a []) = a
last (NonEmpty _ as) = U.last as

push :: forall a. a -> NonEmpty a -> NonEmpty a
push x (NonEmpty a as) = NonEmpty x (a `A.cons` as)

pop :: forall a. NonEmpty a -> Array a
pop (NonEmpty a []) = []
pop (NonEmpty a as) = a `A.cons` (pop_ as)

(<|) :: forall a. a -> NonEmpty a -> NonEmpty a
(<|) a as = a :| toArray as

take :: forall a. Int -> NonEmpty a -> Array a
take 0 _ = []
take 1 (NonEmpty a _) = [a]
take n (NonEmpty a as) = a `A.cons` (A.take (n - 1) as)

drop :: forall a. Int -> NonEmpty a -> Array a
drop 0 nel = toArray nel
drop 1 (NonEmpty _ as) = as
drop n (NonEmpty _ as) = A.drop (n-1) as

filter :: forall a. (a -> Boolean) -> NonEmpty a -> Array a
filter p as = A.filter p (toArray as)

singleton :: forall a. a -> NonEmpty a
singleton a = NonEmpty a []

nub :: forall a. (Eq a) => NonEmpty a -> NonEmpty a
nub = lift A.nub

nubBy :: forall a. (a -> a -> Boolean) -> NonEmpty a -> NonEmpty a
nubBy f = lift (A.nubBy f)

lift :: forall a b. (Array a -> Array b) -> NonEmpty a -> NonEmpty b
lift f as = fromArray_ $ f (toArray as)

concatMap :: forall a b. (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
concatMap f as = fromArray_ $ A.concatMap g (toArray as)
  where g a = toArray $ f a

infixl 8 !!
(!!) :: forall a. NonEmpty a -> Int -> Maybe a
(!!) (NonEmpty a _) 0 = Just a
(!!) (NonEmpty _ as) n = A.(!!) as (n-1)

reverse :: forall a. NonEmpty a -> NonEmpty a
reverse as = fromArray_ $ A.reverse $ toArray as

reducer :: forall a. (a -> a -> a) -> NonEmpty a -> a
reducer f (NonEmpty a as) = foldr f a as

reducel :: forall a. (a -> a -> a) -> NonEmpty a -> a
reducel f (NonEmpty a as) = foldl f a as

sconcat :: forall a. (Semigroup a) => NonEmpty a -> a
sconcat = reducel (<>)

fromArray_ :: forall a. Array a -> NonEmpty a
fromArray_ as = U.head as :| U.tail as

foreign import pop_ :: forall a. Array a -> Array a
