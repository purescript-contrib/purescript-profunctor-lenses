## Module Data.Lens.Iso

This module defines functions for working with isomorphisms.

#### `iso`

``` purescript
iso :: forall s t a b. (s -> a) -> (b -> t) -> Iso s t a b
```

Create an `Iso` from a pair of morphisms.


