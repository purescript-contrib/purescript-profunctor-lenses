## Module Data.Lens.Types

This module defines types for working with lenses.

#### `Optic`

``` purescript
type Optic p s t a b = p a b -> p s t
```

A general-purpose Data.Lens.

#### `OpticP`

``` purescript
type OpticP p s a = Optic p s s a a
```

#### `Iso`

``` purescript
type Iso s t a b = forall p. (Profunctor p) => Optic p s t a b
```

A generalized isomorphism.

#### `IsoP`

``` purescript
type IsoP s a = Iso s s a a
```

#### `AnIso`

``` purescript
type AnIso s t a b = Optic (Exchange a b) s t a b
```

#### `AnIsoP`

``` purescript
type AnIsoP s a = AnIso s s a a
```

#### `Lens`

``` purescript
type Lens s t a b = forall p. (Strong p) => Optic p s t a b
```

A lens.

#### `LensP`

``` purescript
type LensP s a = Lens s s a a
```

#### `ALens`

``` purescript
type ALens s t a b = Optic (Shop a b) s t a b
```

#### `ALensP`

``` purescript
type ALensP s a = ALens s s a a
```

#### `Prism`

``` purescript
type Prism s t a b = forall p. (Choice p) => Optic p s t a b
```

A prism.

#### `PrismP`

``` purescript
type PrismP s a = Prism s s a a
```

#### `APrism`

``` purescript
type APrism s t a b = Optic (Market a b) s t a b
```

#### `APrismP`

``` purescript
type APrismP s a = APrism s s a a
```

#### `Traversal`

``` purescript
type Traversal s t a b = forall p. (Wander p) => Optic p s t a b
```

A traversal.

#### `TraversalP`

``` purescript
type TraversalP s a = Traversal s s a a
```

#### `Getter`

``` purescript
type Getter s t a b = Fold a s t a b
```

A getter.

#### `GetterP`

``` purescript
type GetterP s a = Getter s s a a
```

#### `Setter`

``` purescript
type Setter s t a b = Optic Function s t a b
```

A setter.

#### `SetterP`

``` purescript
type SetterP s a = Setter s s a a
```

#### `Review`

``` purescript
type Review s t a b = Optic Tagged s t a b
```

A review.

#### `ReviewP`

``` purescript
type ReviewP s a = Review s s a a
```

#### `Fold`

``` purescript
type Fold r s t a b = Optic (Forget r) s t a b
```

A fold.

#### `FoldP`

``` purescript
type FoldP r s a = Fold r s s a a
```

#### `IndexedOptic`

``` purescript
type IndexedOptic p i s t a b = Indexed p i a b -> p s t
```

An indexed optic.

#### `IndexedOpticP`

``` purescript
type IndexedOpticP p i s a = IndexedOptic p i s s a a
```

#### `IndexedTraversal`

``` purescript
type IndexedTraversal i s t a b = forall p. (Wander p) => IndexedOptic p i s t a b
```

An indexed traversal.

#### `IndexedTraversalP`

``` purescript
type IndexedTraversalP i s a = IndexedTraversal i s s a a
```

#### `IndexedFold`

``` purescript
type IndexedFold r i s t a b = IndexedOptic (Forget r) i s t a b
```

An indexed fold.

#### `IndexedFoldP`

``` purescript
type IndexedFoldP r i s a = IndexedFold r i s s a a
```

#### `IndexedGetter`

``` purescript
type IndexedGetter i s t a b = IndexedFold a i s t a b
```

An indexed getter.

#### `IndexedGetterP`

``` purescript
type IndexedGetterP i s a = IndexedGetter i s s a a
```

#### `IndexedSetter`

``` purescript
type IndexedSetter i s t a b = IndexedOptic Function i s t a b
```

An indexed setter.

#### `IndexedSetterP`

``` purescript
type IndexedSetterP i s a = IndexedSetter i s s a a
```


