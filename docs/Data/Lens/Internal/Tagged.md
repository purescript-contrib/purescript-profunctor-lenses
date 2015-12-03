## Module Data.Lens.Internal.Tagged

This module defines the `Tagged` profunctor

#### `Tagged`

``` purescript
newtype Tagged a b
  = Tagged b
```

##### Instances
``` purescript
Profunctor Tagged
Choice Tagged
```

#### `unTagged`

``` purescript
unTagged :: forall a b. Tagged a b -> b
```


