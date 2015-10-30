-- | This module defines functions for working with getters.

module Data.Lens.Getter
  ( (^.)
  , view, to
  , module Data.Lens.Types
  ) where

import Prelude ((<<<))

import Data.Const (Const(..), getConst)
import Data.Functor.Contravariant (Contravariant, cmap)
import Data.Profunctor.Star (Star(..), runStar)

import Data.Lens.Types (Getter(), Optic())

infixl 8 ^.

-- | View the focus of a `Getter`.
view :: forall s t a b. Getter s t a b -> s -> a
view l s = getConst (runStar (l (Star Const)) s)

-- | Synonym for `view`, flipped.
(^.) :: forall s t a b. s -> Getter s t a b -> a
(^.) s l = view l s

-- | Convert a function into a getter.
to :: forall s a f. (Contravariant f) => (s -> a) -> Optic (Star f) s s a a
to f p = Star (cmap f <<< runStar p <<< f)
