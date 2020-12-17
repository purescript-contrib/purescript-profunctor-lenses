-- | This module defines functions for working with setters.
module Data.Lens.Setter
  ( (%~)
  , over
  , iover
  , (.~)
  , set
  , (+~)
  , addOver
  , (-~)
  , subOver
  , (*~)
  , mulOver
  , (//~)
  , divOver
  , (||~)
  , disjOver
  , (&&~)
  , conjOver
  , (<>~)
  , appendOver
  , (++~)
  , (?~)
  , setJust
  , (.=)
  , assign
  , (%=)
  , modifying
  , (+=)
  , addModifying
  , (*=)
  , mulModifying
  , (-=)
  , subModifying
  , (//=)
  , divModifying
  , (||=)
  , disjModifying
  , (&&=)
  , conjModifying
  , (<>=)
  , appendModifying
  , (++=)
  , (?=)
  , assignJust
  , fuseSetters
  , cTuple
  , module Data.Lens.Types
  ) where

import Prelude
import Control.Monad.State.Class (class MonadState, modify)
import Data.Lens.Types (IndexedSetter, Indexed(..), Setter, Setter')
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry, Tuple(..), fst, snd)

infixr 4 over as %~

infixr 4 set as .~

infixr 4 addOver as +~

infixr 4 subOver as -~

infixr 4 mulOver as *~

infixr 4 divOver as //~

infixr 4 disjOver as ||~

infixr 4 conjOver as &&~

infixr 4 appendOver as <>~

infixr 4 appendOver as ++~

infixr 4 setJust as ?~

infix 4 assign as .=

infix 4 modifying as %=

infix 4 addModifying as +=

infix 4 mulModifying as *=

infix 4 subModifying as -=

infix 4 divModifying as //=

infix 4 disjModifying as ||=

infix 4 conjModifying as &&=

infix 4 appendModifying as <>=

infix 4 appendModifying as ++=

infix 4 assignJust as ?=

-- | Apply a function to the foci of a `Setter`.
over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over l = l

-- | Apply a function to the foci of a `Setter` that may vary with the index.
iover :: forall i s t a b. IndexedSetter i s t a b -> (i -> a -> b) -> s -> t
iover l f = l (Indexed $ uncurry f)

-- | Set the foci of a `Setter` to a constant value.
set :: forall s t a b. Setter s t a b -> b -> s -> t
set l b = over l (const b)

addOver :: forall s t a. Semiring a => Setter s t a a -> a -> s -> t
addOver p = over p <<< add

mulOver :: forall s t a. Semiring a => Setter s t a a -> a -> s -> t
mulOver p = over p <<< flip mul

subOver :: forall s t a. Ring a => Setter s t a a -> a -> s -> t
subOver p = over p <<< flip sub

divOver :: forall s t a. EuclideanRing a => Setter s t a a -> a -> s -> t
divOver p = over p <<< flip div

disjOver :: forall s t a. HeytingAlgebra a => Setter s t a a -> a -> s -> t
disjOver p = over p <<< flip disj

conjOver :: forall s t a. HeytingAlgebra a => Setter s t a a -> a -> s -> t
conjOver p = over p <<< flip conj

appendOver :: forall s t a. Semigroup a => Setter s t a a -> a -> s -> t
appendOver p = over p <<< flip append

setJust :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
setJust p = set p <<< Just

-- Stateful
-- | Set the foci of a `Setter` in a monadic state to a constant value.
assign :: forall s a b m. MonadState s m => Setter s s a b -> b -> m Unit
assign p b = void (modify (set p b))

-- | Modify the foci of a `Setter` in a monadic state.
modifying :: forall s a b m. MonadState s m => Setter s s a b -> (a -> b) -> m Unit
modifying p f = void (modify (over p f))

addModifying :: forall s a m. MonadState s m => Semiring a => Setter' s a -> a -> m Unit
addModifying p = modifying p <<< add

mulModifying :: forall s a m. MonadState s m => Semiring a => Setter' s a -> a -> m Unit
mulModifying p = modifying p <<< flip mul

subModifying :: forall s a m. MonadState s m => Ring a => Setter' s a -> a -> m Unit
subModifying p = modifying p <<< flip sub

divModifying :: forall s a m. MonadState s m => EuclideanRing a => Setter' s a -> a -> m Unit
divModifying p = modifying p <<< flip div

disjModifying :: forall s a m. MonadState s m => HeytingAlgebra a => Setter' s a -> a -> m Unit
disjModifying p = modifying p <<< flip disj

conjModifying :: forall s a m. MonadState s m => HeytingAlgebra a => Setter' s a -> a -> m Unit
conjModifying p = modifying p <<< flip conj

appendModifying :: forall s a m. MonadState s m => Semigroup a => Setter' s a -> a -> m Unit
appendModifying p = modifying p <<< flip append

assignJust :: forall s a b m. MonadState s m => Setter s s a (Maybe b) -> b -> m Unit
assignJust p = assign p <<< Just

-- | Add two setters as "branches" to a trunk setter.
-- | 
-- | Useful when there are multiple setters acting on the same data structure.
-- | and you need to optimize performance.  For large operations (ie a setter
-- | over an audio file or a photo), this can have a 1.5-2x performance increase
-- | for each fused setter. The performance increase compounds with each nested
-- | fused setter.
-- |
-- | ```purescript
-- |over
-- |  (_2 <<< (fuseSetters _1 (_2 <<< (fuseSetters _1 _2))))
-- |  ( cTuple
-- |    ((+) 55)
-- |    (cTuple (flip (-) 101) ((*) 57))
-- |  )
-- |  (Tuple 0 (Tuple 0 (Tuple 1 2)))
-- | ```
-- |
-- | This yields:
-- |
-- | ```bash
-- | (Tuple 0 (Tuple 55 (Tuple -100 114)))
-- | ```
-- |
-- | Use `cTuple` along with `fuseSetters` to create the tree of functions used
-- | by `over`.
fuseSetters :: forall a b c. Setter' a b -> Setter' a c -> Setter' a (Tuple (b -> b) (c -> c))
fuseSetters ba ca l = (over ba fa <<< over ca fb)
  where
  t = l (Tuple identity identity)

  fa = fst t

  fb = snd t

-- | For use with `fuseSetters`
cTuple :: forall a b c. a -> b -> c -> Tuple a b
cTuple a b _ = Tuple a b
