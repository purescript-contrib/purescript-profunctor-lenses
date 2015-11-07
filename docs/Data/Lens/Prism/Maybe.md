## Module Data.Lens.Prism.Maybe

#### `_Nothing`

``` purescript
_Nothing :: forall a b. Prism (Maybe a) (Maybe b) Unit Unit
```

Prism for the `Nothing` constructor of `Maybe`.

#### `_Just`

``` purescript
_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
```

Prism for the `Just` constructor of `Maybe`.


