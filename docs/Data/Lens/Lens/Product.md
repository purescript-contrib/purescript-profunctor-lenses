## Module Data.Lens.Lens.Product

#### `_1`

``` purescript
_1 :: forall f g h a. Lens (Product f g a) (Product h g a) (f a) (h a)
```

Lens for the first component of a `Product`.

#### `_2`

``` purescript
_2 :: forall f g h a. Lens (Product f g a) (Product f h a) (g a) (h a)
```

Lens for the second component of a `Product`.


