-- | This module defines functions for working with setters.

module Data.Lens.Setter
  ( (%~), (.~), (+~), (-~), (*~), (//~), (||~), (&&~), (<>~), (++~), (?~)
  , (.=), (%=), (+=), (*=), (-=), (//=), (||=), (&&=), (<>=), (++=), (?=)
  , over, set
  , module Data.Lens.Types
  ) where

import Prelude

import Control.Monad.State.Class (MonadState, modify)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)

import Data.Lens.Types (Setter(), SetterP())
import Data.Lens.Types (IndexedSetter(), Indexed(..))

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

infix 4 .=
infix 4 %=
infix 4 +=
infix 4 *=
infix 4 -=
infix 4 //=
infix 4 ||=
infix 4 &&=
infix 4 <>=
infix 4 ++=
infix 4 ?=

-- | Apply a function to the foci of a `Setter`.
over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over l = l

-- | Synonym for `over`.
(%~) :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
(%~) = over

-- | Apply a function to the foci of a `Setter` that may vary with the index.
iover :: forall i s t a b. IndexedSetter i s t a b -> (i -> a -> b) -> s -> t
iover l f = l (Indexed $ uncurry f)

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

-- Stateful

-- | Set the foci of a `Setter` in a monadic state to a constant value.
assign :: forall s a b m. (MonadState s m) => Setter s s a b -> b -> m Unit
assign p b = modify (set p b)

-- | Modify the foci of a `Setter` in a monadic state.
modifying :: forall s a b m. (MonadState s m) => Setter s s a b -> (a -> b) -> m Unit
modifying p f = modify (over p f)

-- | Synonym for `assign`
(.=) :: forall s a b m. (MonadState s m) => Setter s s a b -> b -> m Unit
(.=) = assign

-- | Synonym for `modifying`
(%=) :: forall s a b m. (MonadState s m) => Setter s s a b -> (a -> b) -> m Unit
(%=) = modifying

(+=) :: forall s a m. (MonadState s m, Semiring a) => SetterP s a -> a -> m Unit
(+=) p = modifying p <<< add

(*=) :: forall s a m. (MonadState s m, Semiring a) => SetterP s a -> a -> m Unit
(*=) p = modifying p <<< flip mul

(-=) :: forall s a m. (MonadState s m, Ring a) => SetterP s a -> a -> m Unit
(-=) p = modifying p <<< flip sub

(//=) :: forall s a m. (MonadState s m, DivisionRing a) => SetterP s a -> a -> m Unit
(//=) p = modifying p <<< flip div

(||=) :: forall s a m. (MonadState s m, BooleanAlgebra a) => SetterP s a -> a -> m Unit
(||=) p = modifying p <<< flip disj

(&&=) :: forall s a m. (MonadState s m, BooleanAlgebra a) => SetterP s a -> a -> m Unit
(&&=) p = modifying p <<< flip conj

(<>=) :: forall s a m. (MonadState s m, Semigroup a) => SetterP s a -> a -> m Unit
(<>=) p = modifying p <<< flip append

(++=) :: forall s a m. (MonadState s m, Semigroup a) => SetterP s a -> a -> m Unit
(++=) p = modifying p <<< flip append

(?=) :: forall s a b m. (MonadState s m) => Setter s s a (Maybe b) -> b -> m Unit
(?=) p = assign p <<< Just
