## Module Data.Lens.Common

This module defines common lenses and prisms.

#### `_Nothing`

``` purescript
_Nothing :: forall a b. Prism (Maybe a) (Maybe b) Unit Unit
```

#### `_Just`

``` purescript
_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
```

Prism for the `Just` constructor of `Maybe`.

#### `_Left`

``` purescript
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
```

Prism for the `Left` constructor of `Either`.

#### `_Right`

``` purescript
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
```

Prism for the `Right` constructor of `Either`.

#### `_1`

``` purescript
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
```

Lens for the first component of a `Tuple`.

#### `_2`

``` purescript
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
```

Lens for the second component of a `Tuple`.

#### `united`

``` purescript
united :: forall a. LensP a Unit
```

There is a `Unit` in everything.

#### `devoid`

``` purescript
devoid :: forall a. LensP Void a
```

There is everything in `Void`.


