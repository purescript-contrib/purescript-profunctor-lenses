## Module Data.Lens.Internal.Indexed

This module defines the `Indexed` profunctor.

#### `Indexed`

``` purescript
newtype Indexed p i s t
  = Indexed (p (Tuple i s) t)
```

Profunctor used for `IndexedOptic`s.

##### Instances
``` purescript
instance indexedProfunctor :: (Profunctor p) => Profunctor (Indexed p i)
instance indexedStrong :: (Strong p) => Strong (Indexed p i)
instance indexedChoice :: (Choice p) => Choice (Indexed p i)
```

#### `fromIndexed`

``` purescript
fromIndexed :: forall p i s t. Indexed p i s t -> p (Tuple i s) t
```

Unwrap a value of type `Indexed`.


