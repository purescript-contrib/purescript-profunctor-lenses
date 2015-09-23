## Module Data.Lens.Internal.Void

This module defines the empty `Void` type.

#### `Void`

``` purescript
data Void
```

#### `absurd`

``` purescript
absurd :: forall a. Void -> a
```

#### `coerce`

``` purescript
coerce :: forall f a b. (Contravariant f, Functor f) => f a -> f b
```


