# Module Documentation

## Module Data.Array.NonEmpty

#### `NonEmpty`

``` purescript
data NonEmpty a
  = NonEmpty a [a]
```


#### `showNonEmpty`

``` purescript
instance showNonEmpty :: (Show a) => Show (NonEmpty a)
```


#### `eqNonEmpty`

``` purescript
instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a)
```


#### `functorNonEmpty`

``` purescript
instance functorNonEmpty :: Functor NonEmpty
```


#### `applyNonEmpty`

``` purescript
instance applyNonEmpty :: Apply NonEmpty
```


#### `applicativeNonEmpty`

``` purescript
instance applicativeNonEmpty :: Applicative NonEmpty
```


#### `bindNonEmpty`

``` purescript
instance bindNonEmpty :: Bind NonEmpty
```


#### `monadNonEmpty`

``` purescript
instance monadNonEmpty :: Monad NonEmpty
```


#### `semigroupNonEmpty`

``` purescript
instance semigroupNonEmpty :: Semigroup (NonEmpty a)
```


#### `foldableNonEmpty`

``` purescript
instance foldableNonEmpty :: Foldable NonEmpty
```


#### `traversableNonEmpty`

``` purescript
instance traversableNonEmpty :: T.Traversable NonEmpty
```


#### `(:|)`

``` purescript
(:|) :: forall a. a -> [a] -> NonEmpty a
```


#### `toArray`

``` purescript
toArray :: forall a. NonEmpty a -> [a]
```


#### `length`

``` purescript
length :: forall a. NonEmpty a -> Number
```


#### `head`

``` purescript
head :: forall a. NonEmpty a -> a
```


#### `tail`

``` purescript
tail :: forall a. NonEmpty a -> [a]
```


#### `last`

``` purescript
last :: forall a. NonEmpty a -> a
```


#### `push`

``` purescript
push :: forall a. a -> NonEmpty a -> NonEmpty a
```


#### `pop`

``` purescript
pop :: forall a. NonEmpty a -> [a]
```


#### `(<|)`

``` purescript
(<|) :: forall a. a -> NonEmpty a -> NonEmpty a
```


#### `take`

``` purescript
take :: forall a. Number -> NonEmpty a -> [a]
```


#### `drop`

``` purescript
drop :: forall a. Number -> NonEmpty a -> [a]
```


#### `map`

``` purescript
map :: forall a b. (a -> b) -> NonEmpty a -> NonEmpty b
```


#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> NonEmpty a -> [a]
```


#### `singleton`

``` purescript
singleton :: forall a. a -> NonEmpty a
```


#### `nub`

``` purescript
nub :: forall a. (Eq a) => NonEmpty a -> NonEmpty a
```


#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> NonEmpty a -> NonEmpty a
```


#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
```


#### `(!!)`

``` purescript
(!!) :: forall a. NonEmpty a -> Number -> Maybe a
```


#### `append`

``` purescript
append :: forall a. NonEmpty a -> NonEmpty a -> NonEmpty a
```


#### `reverse`

``` purescript
reverse :: forall a. NonEmpty a -> NonEmpty a
```


#### `reducer`

``` purescript
reducer :: forall a. (a -> a -> a) -> NonEmpty a -> a
```


#### `reducel`

``` purescript
reducel :: forall a. (a -> a -> a) -> NonEmpty a -> a
```



## Module Data.Array.NonEmpty.Unsafe

#### `fromArray`

``` purescript
fromArray :: forall a. [a] -> NonEmpty a
```