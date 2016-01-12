-- | This module defines functions for working with getters.

module Data.Lens.Getter
  ( (^.)
  , view, to, use
  , module Data.Lens.Types
  ) where

import Prelude (id, (<<<))

import Data.Const (Const(..), getConst)
import Data.Functor.Contravariant (Contravariant, cmap)
import Data.Profunctor.Star (Star(..), runStar)
import Control.Monad.State.Class (MonadState, gets)

import Data.Lens.Internal.Forget (Forget (..), runForget)
import Data.Lens.Types (Getter(), Optic())

infixl 8 ^.

-- | View the focus of a `Getter`.
view :: forall s t a b. Getter s t a b -> s -> a
view l = runForget (l (Forget id))

-- | Synonym for `view`, flipped.
(^.) :: forall s t a b. s -> Getter s t a b -> a
(^.) s l = view l s

-- | Convert a function into a getter.
to :: forall s a f. (Contravariant f) => (s -> a) -> Optic (Star f) s s a a
to f p = Star (cmap f <<< runStar p <<< f)

use :: forall s t a b m. (MonadState s m) => Getter s t a b -> m a
use p = gets (^. p)
