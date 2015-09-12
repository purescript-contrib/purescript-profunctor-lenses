-- | This module defines functions for working with setters.

module Data.Lens.Setter
  ( (%~), (.~), (+~), (-~), (*~), (//~), (||~), (&&~), (<>~), (++~), (?~)
  , over, set
  ) where

import Prelude
import Data.Const
import Data.Profunctor.Star
import Data.Lens.Types
import Data.Maybe

infixr 4 %~
infixr 4 .~
infixr 4 +~
infixr 4 -~
infixr 4 *~
infixr 4 //~
infixr 4 ||~
infixr 4 &&~
infixr 4 <>~
infixr 4 ++~
infixr 4 ?~

-- | Apply a function to the foci of a `Setter`.
over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over l = l

-- | Synonym for `over`.
(%~) :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
(%~) = over

-- | Set the foci of a `Setter` to a constant value.
set :: forall s t a b. Setter s t a b -> b -> s -> t
set l b = over l (const b)

-- | Synonym for `set`.
(.~) :: forall s t a b. Setter s t a b -> b -> s -> t
(.~) = set

(+~) :: forall s t a a. (Semiring a) => Setter s t a a -> a -> s -> t
(+~) p = over p <<< flip (+)

(*~) :: forall s t a a. (Semiring a) => Setter s t a a -> a -> s -> t
(*~) p = over p <<< flip (*)

(-~) :: forall s t a a. (Ring a) => Setter s t a a -> a -> s -> t
(-~) p = over p <<< flip (-)

(//~) :: forall s t a a. (DivisionRing a) => Setter s t a a -> a -> s -> t
(//~) p = over p <<< flip (/)

(||~) :: forall s t a a. (BooleanAlgebra a) => Setter s t a a -> a -> s -> t
(||~) p = over p <<< flip (||)

(&&~) :: forall s t a a. (BooleanAlgebra a) => Setter s t a a -> a -> s -> t
(&&~) p = over p <<< flip (&&)

(<>~) :: forall s t a a. (Semigroup a) => Setter s t a a -> a -> s -> t
(<>~) p = over p <<< flip (<>)

(++~) :: forall s t a a. (Semigroup a) => Setter s t a a -> a -> s -> t
(++~) p = over p <<< flip (++)

(?~) :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
(?~) p = set p <<< Just
