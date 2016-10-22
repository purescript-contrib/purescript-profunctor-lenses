-- | This module defines functions for working with getters.
module Data.Lens.Getter
  ( (^.), viewOn
  , view, to, use, iview, iuse
  , module Data.Lens.Types
  ) where

import Prelude

import Control.Monad.State.Class (class MonadState, gets)

import Data.Lens.Internal.Forget (Forget(..))
import Data.Lens.Types (Getter, Fold, Optic, IndexedGetter, Indexed(..), IndexedFold)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)

infixl 8 viewOn as ^.

-- | View the focus of a `Getter`.
view :: forall s t a b. Getter s t a b -> s -> a
view l = unwrap (l (Forget id))

-- | View the focus of a `Getter` and its index.
iview :: forall i s t a b. IndexedFold (Tuple i a) i s t a b -> s -> Tuple i a
iview l = unwrap (l (Indexed $ Forget id))

-- | Synonym for `view`, flipped.
viewOn :: forall s t a b. s -> Getter s t a b -> a
viewOn s l = view l s

-- | Convert a function into a getter.
to :: forall r s t a b. (s -> a) -> Fold r s t a b
to f p = Forget (unwrap p <<< f)

-- | View the focus of a `Getter` in the state of a monad.
use :: forall s t a b m. MonadState s m => Getter s t a b -> m a
use p = gets (_ ^. p)

-- | View the focus of a `Getter` and its index in the state of a monad.
iuse
  :: forall i s t a b m
   . MonadState s m
  => IndexedFold (Tuple i a) i s t a b -> m (Tuple i a)
iuse p = gets (iview p)
