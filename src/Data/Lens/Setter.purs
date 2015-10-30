-- | This module defines functions for working with setters.

module Data.Lens.Setter
  ( (%~), (.~), (+~), (-~), (*~), (//~), (||~), (&&~), (<>~), (++~), (?~)
  , over, set
  , module Data.Lens.Types
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Data.Lens.Types (Setter(), SetterP())

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

(+~) :: forall s t a. (Semiring a) => Setter s t a a -> a -> s -> t
(+~) p = over p <<< add

(*~) :: forall s t a. (Semiring a) => Setter s t a a -> a -> s -> t
(*~) p = over p <<< flip mul

(-~) :: forall s t a. (Ring a) => Setter s t a a -> a -> s -> t
(-~) p = over p <<< flip sub

(//~) :: forall s t a. (DivisionRing a) => Setter s t a a -> a -> s -> t
(//~) p = over p <<< flip div

(||~) :: forall s t a. (BooleanAlgebra a) => Setter s t a a -> a -> s -> t
(||~) p = over p <<< flip disj

(&&~) :: forall s t a. (BooleanAlgebra a) => Setter s t a a -> a -> s -> t
(&&~) p = over p <<< flip conj

(<>~) :: forall s t a. (Semigroup a) => Setter s t a a -> a -> s -> t
(<>~) p = over p <<< flip append

(++~) :: forall s t a. (Semigroup a) => Setter s t a a -> a -> s -> t
(++~) p = over p <<< flip append

(?~) :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
(?~) p = set p <<< Just
