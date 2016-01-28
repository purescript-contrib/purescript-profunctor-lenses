## Module Data.Lens.Getter

This module defines functions for working with getters.

#### `view`

``` purescript
view :: forall s t a b. Getter s t a b -> s -> a
```

View the focus of a `Getter`.

#### `iview`

``` purescript
iview :: forall i s t a b. IndexedFold (Tuple i a) i s t a b -> s -> Tuple i a
```

View the focus of a `Getter` and its index.

#### `(^.)`

``` purescript
(^.) :: forall s t a b. s -> Getter s t a b -> a
```

_left-associative / precedence 8_

Synonym for `view`, flipped.

#### `to`

``` purescript
to :: forall s a f. (Contravariant f) => (s -> a) -> Optic (Star f) s s a a
```

Convert a function into a getter.

#### `use`

``` purescript
use :: forall s t a b m. (MonadState s m) => Getter s t a b -> m a
```

View the focus of a `Getter` in the state of a monad.

#### `iuse`

``` purescript
iuse :: forall i s t a b m. (MonadState s m) => IndexedFold (Tuple i a) i s t a b -> m (Tuple i a)
```

View the focus of a `Getter` and its index in the state of a monad.


