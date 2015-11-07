## Module Data.Lens.Prism

This module defines functions for working with prisms.

#### `prism`

``` purescript
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
```

Create a `Prism` from a constructor/pattern pair.

#### `prism'`

``` purescript
prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> PrismP s a
```

#### `review`

``` purescript
review :: forall s t a b. Review s t a b -> b -> t
```

Review a value through a `Prism`.

#### `nearly`

``` purescript
nearly :: forall a. a -> (a -> Boolean) -> PrismP a Unit
```

#### `only`

``` purescript
only :: forall a. (Eq a) => a -> Prism a a Unit Unit
```

#### `clonePrism`

``` purescript
clonePrism :: forall s t a b. APrism s t a b -> Prism s t a b
```

#### `withPrism`

``` purescript
withPrism :: forall s t a b r. APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
```

#### `matching`

``` purescript
matching :: forall s t a b. APrism s t a b -> s -> Either t a
```

#### `is`

``` purescript
is :: forall s t a b r. (BooleanAlgebra r) => APrism s t a b -> s -> r
```

#### `isn't`

``` purescript
isn't :: forall s t a b r. (BooleanAlgebra r) => APrism s t a b -> s -> r
```


