## Module Data.Lens.Prism.Coproduct

#### `_Left`

``` purescript
_Left :: forall f g h a. Prism (Coproduct f g a) (Coproduct h g a) (f a) (h a)
```

Prism for the `left` of a `Coproduct`.

#### `_Right`

``` purescript
_Right :: forall f g h a. Prism (Coproduct f g a) (Coproduct f h a) (g a) (h a)
```

Prism for the `right` of a `Coproduct`.


