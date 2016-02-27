-- | This module defines functions for working with getters.

module Data.Lens.Getter
  ( (^.)
  , view, to, use, iview, iuse
  , module Data.Lens.Types
  ) where

import Prelude (id, (<<<), ($))

import Data.Const (Const(..), getConst)
import Data.Functor.Contravariant (Contravariant, cmap)
import Data.Profunctor (Profunctor, lmap)
import Data.Profunctor.Star (Star(..), runStar)
import Data.Tuple (Tuple (..))
import Control.Monad.State.Class (MonadState, gets)

import Data.Lens.Internal.Forget (Forget (..), runForget)
import Data.Lens.Types (Getter(), Fold(), Optic())
import Data.Lens.Types (IndexedGetter(), Indexed (..))
import Data.Lens.Types (IndexedFold())

infixl 8 ^.

-- | View the focus of a `Getter`.
view :: forall s t a b. Getter s t a b -> s -> a
view l = runForget (l (Forget id))

-- | View the focus of a `Getter` and its index.
iview :: forall i s t a b. IndexedFold (Tuple i a) i s t a b -> s -> Tuple i a
iview l = runForget (l (Indexed $ Forget id))

-- | Synonym for `view`, flipped.
(^.) :: forall s t a b. s -> Getter s t a b -> a
(^.) s l = view l s

-- | Convert a function into a getter.
to :: forall r s t a b. (s -> a) -> Fold r s t a b
to f p = Forget (runForget p <<< f)

-- | View the focus of a `Getter` in the state of a monad.
use :: forall s t a b m. (MonadState s m) => Getter s t a b -> m a
use p = gets (^. p)

-- | View the focus of a `Getter` and its index in the state of a monad.
iuse
  :: forall i s t a b m. (MonadState s m)
  => IndexedFold (Tuple i a) i s t a b -> m (Tuple i a)
iuse p = gets (iview p)
