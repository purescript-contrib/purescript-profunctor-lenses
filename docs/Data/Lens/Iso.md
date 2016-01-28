## Module Data.Lens.Iso

This module defines functions for working with isomorphisms.

#### `iso`

``` purescript
iso :: forall s t a b. (s -> a) -> (b -> t) -> Iso s t a b
```

Create an `Iso` from a pair of morphisms.

#### `withIso`

``` purescript
withIso :: forall s t a b r. AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
```

Extracts the pair of morphisms from an isomorphism.

#### `cloneIso`

``` purescript
cloneIso :: forall s t a b. AnIso s t a b -> Iso s t a b
```

Extracts an `Iso` from `AnIso`.

#### `re`

``` purescript
re :: forall p s t a b. Optic (Re p a b) s t a b -> Optic p b a t s
```

Reverses an optic.

#### `au`

``` purescript
au :: forall s t a b e. AnIso s t a b -> ((b -> t) -> e -> s) -> e -> a
```

#### `auf`

``` purescript
auf :: forall s t a b e r p. (Profunctor p) => AnIso s t a b -> (p r a -> e -> b) -> p r s -> e -> t
```

#### `under`

``` purescript
under :: forall s t a b. AnIso s t a b -> (t -> s) -> b -> a
```

#### `curried`

``` purescript
curried :: forall a b c d e f. Iso (Tuple a b -> c) (Tuple d e -> f) (a -> b -> c) (d -> e -> f)
```

#### `uncurried`

``` purescript
uncurried :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (Tuple a b -> c) (Tuple d e -> f)
```

#### `flipped`

``` purescript
flipped :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (b -> a -> c) (e -> d -> f)
```


