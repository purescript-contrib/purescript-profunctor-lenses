## Module Data.Lens.Prism.Either

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


