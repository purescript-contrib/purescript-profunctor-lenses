## Module Data.Lens.Setter

This module defines functions for working with setters.

#### `over`

``` purescript
over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
```

Apply a function to the foci of a `Setter`.

#### `(%~)`

``` purescript
(%~) :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
```

_right-associative / precedence 4_

Synonym for `over`.

#### `set`

``` purescript
set :: forall s t a b. Setter s t a b -> b -> s -> t
```

Set the foci of a `Setter` to a constant value.

#### `(.~)`

``` purescript
(.~) :: forall s t a b. Setter s t a b -> b -> s -> t
```

_right-associative / precedence 4_

Synonym for `set`.

#### `(+~)`

``` purescript
(+~) :: forall s t a. (Semiring a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(*~)`

``` purescript
(*~) :: forall s t a. (Semiring a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(-~)`

``` purescript
(-~) :: forall s t a. (Ring a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(//~)`

``` purescript
(//~) :: forall s t a. (DivisionRing a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(||~)`

``` purescript
(||~) :: forall s t a. (BooleanAlgebra a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(&&~)`

``` purescript
(&&~) :: forall s t a. (BooleanAlgebra a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(<>~)`

``` purescript
(<>~) :: forall s t a. (Semigroup a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(++~)`

``` purescript
(++~) :: forall s t a. (Semigroup a) => Setter s t a a -> a -> s -> t
```

_right-associative / precedence 4_

#### `(?~)`

``` purescript
(?~) :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
```

_right-associative / precedence 4_


