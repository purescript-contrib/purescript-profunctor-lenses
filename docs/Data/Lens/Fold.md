## Module Data.Lens.Fold

This module defines functions for working with getters.

#### `preview`

``` purescript
preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
```

Previews the first value of a fold, if there is any.

#### `(^?)`

``` purescript
(^?) :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
```

_left-associative / precedence 8_

Synonym for `preview`, flipped.

#### `foldOf`

``` purescript
foldOf :: forall s t a b. Fold a s t a b -> s -> a
```

Folds all foci of a `Fold` to one. Note that this is the same as `view`.

#### `foldMapOf`

``` purescript
foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
```

Maps and then folds all foci of a `Fold`.

#### `foldrOf`

``` purescript
foldrOf :: forall s t a b r. Fold (Endo r) s t a b -> (a -> r -> r) -> r -> s -> r
```

Right fold over a `Fold`.

#### `foldlOf`

``` purescript
foldlOf :: forall s t a b r. Fold (Dual (Endo r)) s t a b -> (r -> a -> r) -> r -> s -> r
```

Left fold over a `Fold`.

#### `toListOf`

``` purescript
toListOf :: forall s t a b. Fold (Endo (List a)) s t a b -> s -> List a
```

Collects the foci of a `Fold` into a list.

#### `(^..)`

``` purescript
(^..) :: forall s t a b. s -> Fold (Endo (List a)) s t a b -> List a
```

_left-associative / precedence 8_

Synonym for `toListOf`, reversed.

#### `has`

``` purescript
has :: forall s t a b r. (BooleanAlgebra r) => Fold (Disj r) s t a b -> s -> r
```

Determines whether a `Fold` has at least one focus.

#### `hasn't`

``` purescript
hasn't :: forall s t a b r. (BooleanAlgebra r) => Fold (Conj r) s t a b -> s -> r
```

Determines whether a `Fold` does not have a focus.

#### `filtered`

``` purescript
filtered :: forall p a. (Choice p) => (a -> Boolean) -> OpticP p a a
```

Filters on a predicate.

#### `replicated`

``` purescript
replicated :: forall r a b t f. (Applicative f, Contravariant f) => Int -> Optic (Star f) a b a t
```

Replicates the elements of a fold.


