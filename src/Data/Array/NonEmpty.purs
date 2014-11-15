module Data.Array.NonEmpty
  (
    NonEmpty(..)
    ,(:|)
    ,(!!)
    ,toArray
    ,length
    ,head
    ,tail
    ,last
    ,pop
    ,(<|)
    ,take
    ,drop
    ,map
    ,filter
    ,singleton
    ,nub
    ,nubBy
    ,concatMap
    ,append
    ,reverse
    ,reducer
    ,reducel
  ) where

import qualified Data.Array (append, drop, take, map, filter, nub, nubBy, concatMap, (!!), length, reverse) as A
import qualified Data.Array.Unsafe (last) as AU
import qualified Data.Foldable (Foldable, foldr, foldl, foldMap, foldrArray, foldlArray) as F
import Data.Maybe (Maybe(..))

data NonEmpty a = NonEmpty a [a]

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
    show (NonEmpty a as) = (show a) ++ " :| " ++ (show as)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  (==) (NonEmpty l ls) (NonEmpty r rs) = l == r && ls == rs
  (/=) l r = not (l == r)

instance functorNonEmpty :: Functor NonEmpty where
  (<$>) = map

instance applyNonEmpty :: Apply NonEmpty where
  (<*>) x y = fromArray_ $ (toArray x) <*> (toArray y)

instance applicativeNonEmpty :: Applicative NonEmpty where
  pure = singleton

instance bindNonEmpty :: Bind NonEmpty where
  (>>=) = flip concatMap

instance monadNonEmpty :: Monad NonEmpty

instance semigroupNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a) where
  (<>) = append

infix 5 :|
(:|) :: forall a. a -> [a] -> NonEmpty a
(:|) a as = NonEmpty a as

toArray :: forall a. NonEmpty a -> [a]
toArray (NonEmpty a as) = a:as

length :: forall a. NonEmpty a -> Number
length (NonEmpty _ as) = 1 + A.length as

head :: forall a. NonEmpty a -> a
head (NonEmpty a _) = a

tail :: forall a. NonEmpty a -> [a]
tail (NonEmpty _ as) = as

last :: forall a. NonEmpty a -> a
last (NonEmpty a []) = a
last (NonEmpty _ as) = AU.last as

pop :: forall a. NonEmpty a -> [a]
pop (NonEmpty a as) = a : (pop_ as)

(<|) :: forall a. a -> NonEmpty a -> NonEmpty a
(<|) a as = a :| toArray as

take :: forall a. Number -> NonEmpty a -> [a]
take 0 _ = []
take 1 (NonEmpty a _) = [a]
take n (NonEmpty a as) = a:(A.take (n - 1) as)

drop :: forall a. Number -> NonEmpty a -> [a]
drop 0 nel = toArray nel
drop 1 (NonEmpty _ as) = as
drop n (NonEmpty _ as) = A.drop (n-1) as

map :: forall a b. (a -> b) -> NonEmpty a -> NonEmpty b
map f (NonEmpty a as) = f a :| (A.map f as)

filter :: forall a. (a -> Boolean) -> NonEmpty a -> [a]
filter p as = A.filter p (toArray as)

singleton :: forall a. a -> NonEmpty a
singleton a = NonEmpty a []

nub :: forall a. (Eq a) => NonEmpty a -> NonEmpty a
nub = apply A.nub

nubBy :: forall a. (a -> a -> Boolean) -> NonEmpty a -> NonEmpty a
nubBy f = apply (A.nubBy f)

apply :: forall a b. ([a] -> [b]) -> NonEmpty a -> NonEmpty b
apply f as = fromArray_ $ f (toArray as)

concatMap :: forall a b. (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
concatMap f as = fromArray_ $ A.concatMap g (toArray as)
  where g a = toArray $ f a

fromArray_ (a:as) = a :| as

infixl 8 !!

(!!) :: forall a. NonEmpty a -> Number -> Maybe a
(!!) (NonEmpty a _) 0 = Just a 
(!!) (NonEmpty _ as) n = A.(!!) as (n-1)

append :: forall a. NonEmpty a -> NonEmpty a -> NonEmpty a
append (NonEmpty a as) ys = a :| A.append as (toArray ys)

reverse :: forall a. NonEmpty a -> NonEmpty a
reverse as = fromArray_ $ A.reverse $ toArray as 

foreign import pop_
  """
  function pop_(l) {
    if(l.length == 0) return l;
    var l1 = l.slice();
    l1.pop(); 
    return l1;
  }
  """ :: forall a. [a] -> [a]

reducer :: forall a. (a -> a -> a) -> NonEmpty a -> a
reducer f (NonEmpty a as) = F.foldrArray f a as

reducel :: forall a. (a -> a -> a) -> NonEmpty a -> a
reducel f (NonEmpty a as) = F.foldlArray f a as
