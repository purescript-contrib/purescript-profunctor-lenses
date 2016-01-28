## Module Data.Lens.Indexed

#### `unIndex`

``` purescript
unIndex :: forall p i s t a b. (Profunctor p) => IndexedOptic p i s t a b -> Optic p s t a b
```

Converts an `IndexedOptic` to an `Optic` by forgetting indices.

#### `iwander`

``` purescript
iwander :: forall p i s t a b. (Wander p) => (forall f. (Applicative f) => (i -> a -> f b) -> s -> f t) -> Indexed p i a b -> p s t
```

Converts a `lens`-like indexed traversal to an `IndexedTraversal`.

#### `positions`

``` purescript
positions :: forall p s t a b. (Wander p) => Traversal s t a b -> IndexedOptic p Int s t a b
```

Converts a `Traversal` to an `IndexedTraversal` by using the integer positions as indices.


