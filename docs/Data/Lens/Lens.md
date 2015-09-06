## Module Data.Lens.Lens

This module defines functions for working with lenses.

#### `lens'`

``` purescript
lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
```

Create a `Lens` from an costore-y cothing.

#### `lens`

``` purescript
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
```

Create a `Lens` from a getter/setter pair.

#### `view`

``` purescript
view :: forall s t a b. Lens s t a b -> s -> a
```

View the focus of a `Lens`.

#### `over`

``` purescript
over :: forall s t a b. Lens s t a b -> (a -> b) -> s -> t
```

Apply a function to the focus.


