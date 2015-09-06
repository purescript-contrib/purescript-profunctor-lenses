## Module Data.Lens.Internal.Wander

This module defines the `Wander` type class, which is used to define `Traversal`s.

#### `Wander`

``` purescript
class (Strong p, Choice p) <= Wander p where
  wander :: forall t a b. (Traversable t) => p a b -> p (t a) (t b)
```

##### Instances
``` purescript
instance wanderFunction :: Wander Function
```


