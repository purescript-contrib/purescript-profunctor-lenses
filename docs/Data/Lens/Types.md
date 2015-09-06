## Module Data.Lens.Types

This module defines functions for working with lenses.

#### `Optic`

``` purescript
type Optic p s t a b = p a b -> p s t
```

A general-purpose optic.

#### `Iso`

``` purescript
type Iso s t a b = forall p. (Profunctor p) => Optic p s t a b
```

A generalized isomorphism.

#### `Lens`

``` purescript
type Lens s t a b = forall p. (Strong p) => Optic p s t a b
```

A lens.

#### `Prism`

``` purescript
type Prism s t a b = forall p. (Choice p) => Optic p s t a b
```

A prism.

#### `Traversal`

``` purescript
type Traversal s t a b = forall p. (Wander p) => Optic p s t a b
```

A traversal.


