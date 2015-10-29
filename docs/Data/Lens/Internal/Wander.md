## Module Data.Lens.Internal.Wander

This module defines the `Wander` type class, which is used to define `Traversal`s.

#### `Wander`

``` purescript
class (Strong p, Choice p) <= Wander p where
  wander :: forall s t a b. (forall f. (Applicative f) => (a -> f b) -> s -> f t) -> p a b -> p s t
```

Class for profunctors that support polymorphic traversals.

##### Instances
``` purescript
instance wanderFunction :: Wander Function
instance wanderStar :: (Applicative f) => Wander (Star f)
```


