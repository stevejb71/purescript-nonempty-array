## Module Data.Array.NonEmpty

#### `NonEmpty`

``` purescript
data NonEmpty a
  = NonEmpty a (Array a)
```

##### Instances
``` purescript
instance showNonEmpty :: (Show a) => Show (NonEmpty a)
instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a)
instance functorNonEmpty :: Functor NonEmpty
instance applyNonEmpty :: Apply NonEmpty
instance applicativeNonEmpty :: Applicative NonEmpty
instance bindNonEmpty :: Bind NonEmpty
instance monadNonEmpty :: Monad NonEmpty
instance semigroupNonEmpty :: Semigroup (NonEmpty a)
instance foldableNonEmpty :: Foldable NonEmpty
instance traversableNonEmpty :: Traversable NonEmpty
```

#### `(:|)`

``` purescript
(:|) :: forall a. a -> Array a -> NonEmpty a
```

_non-associative / precedence 5_

#### `toArray`

``` purescript
toArray :: forall a. NonEmpty a -> Array a
```

#### `length`

``` purescript
length :: forall a. NonEmpty a -> Int
```

#### `head`

``` purescript
head :: forall a. NonEmpty a -> a
```

#### `tail`

``` purescript
tail :: forall a. NonEmpty a -> Array a
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
pop :: forall a. NonEmpty a -> Array a
```

#### `(<|)`

``` purescript
(<|) :: forall a. a -> NonEmpty a -> NonEmpty a
```

_left-associative / precedence -1_

#### `take`

``` purescript
take :: forall a. Int -> NonEmpty a -> Array a
```

#### `drop`

``` purescript
drop :: forall a. Int -> NonEmpty a -> Array a
```

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> NonEmpty a -> Array a
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
(!!) :: forall a. NonEmpty a -> Int -> Maybe a
```

_left-associative / precedence 8_

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

#### `sconcat`

``` purescript
sconcat :: forall a. (Semigroup a) => NonEmpty a -> a
```


