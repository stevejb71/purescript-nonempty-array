# Module Documentation

## Module Data.Array.NonEmpty

### Types

    data NonEmpty a where
      NonEmpty :: a -> [a] -> NonEmpty a


### Type Class Instances

    instance applicativeNonEmpty :: Applicative NonEmpty

    instance applyNonEmpty :: Apply NonEmpty

    instance bindNonEmpty :: Bind NonEmpty

    instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a)

    instance functorNonEmpty :: Functor NonEmpty

    instance monadNonEmpty :: Monad NonEmpty

    instance semigroupNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a)

    instance showNonEmpty :: (Show a) => Show (NonEmpty a)


### Values

    (!!) :: forall a. NonEmpty a -> Number -> Maybe a

    (:|) :: forall a. a -> [a] -> NonEmpty a

    (<|) :: forall a. a -> NonEmpty a -> NonEmpty a

    append :: forall a. NonEmpty a -> NonEmpty a -> NonEmpty a

    concatMap :: forall a b. (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b

    drop :: forall a. Number -> NonEmpty a -> [a]

    filter :: forall a. (a -> Boolean) -> NonEmpty a -> [a]

    head :: forall a. NonEmpty a -> a

    last :: forall a. NonEmpty a -> a

    length :: forall a. NonEmpty a -> Number

    map :: forall a b. (a -> b) -> NonEmpty a -> NonEmpty b

    nub :: forall a. (Eq a) => NonEmpty a -> NonEmpty a

    nubBy :: forall a. (a -> a -> Boolean) -> NonEmpty a -> NonEmpty a

    pop :: forall a. NonEmpty a -> [a]

    reducel :: forall a. (a -> a -> a) -> NonEmpty a -> a

    reducer :: forall a. (a -> a -> a) -> NonEmpty a -> a

    reverse :: forall a. NonEmpty a -> NonEmpty a

    singleton :: forall a. a -> NonEmpty a

    tail :: forall a. NonEmpty a -> [a]

    take :: forall a. Number -> NonEmpty a -> [a]

    toArray :: forall a. NonEmpty a -> [a]


## Module Data.Array.NonEmpty.Unsafe

### Values

    fromArray :: forall a. [a] -> NonEmpty a