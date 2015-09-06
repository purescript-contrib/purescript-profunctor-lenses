## Module Data.Lens.Prism

This module defines functions for working with lenses.

#### `prism`

``` purescript
prism :: forall s t a b. (b -> t) -> (s -> Either a t) -> Prism s t a b
```

Create a `Prism` from a constructor/pattern pair.


