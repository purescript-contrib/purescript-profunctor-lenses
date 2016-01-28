## Module Data.Lens.Traversal

This module defines functions for working with traversals.

#### `traversed`

``` purescript
traversed :: forall t a b. (Traversable t) => Traversal (t a) (t b) a b
```

Create a `Traversal` which traverses the elements of a `Traversable` functor.

#### `traverseOf`

``` purescript
traverseOf :: forall f s t a b. (Applicative f) => Optic (Star f) s t a b -> (a -> f b) -> s -> f t
```

Turn a pure profunctor `Traversal` into a `lens`-like `Traversal`.

#### `sequenceOf`

``` purescript
sequenceOf :: forall f s t a. (Applicative f) => Optic (Star f) s t (f a) a -> s -> f t
```

Sequence the foci of a `Traversal`, pulling out an `Applicative` effect.
If you do not need the result, see `sequenceOf_` for `Fold`s.

#### `failover`

``` purescript
failover :: forall f s t a b. (Alternative f) => Optic (Star (Tuple (Disj Boolean))) s t a b -> (a -> b) -> s -> f t
```

Tries to map over a `Traversal`; returns `empty` if the traversal did
not have any new focus.

#### `elementsOf`

``` purescript
elementsOf :: forall p i s t a. (Wander p) => IndexedTraversal i s t a a -> (i -> Boolean) -> IndexedOptic p i s t a a
```

Traverse elements of an `IndexedTraversal` whose index satisfy a predicate.


