-- | This module defines the `Focusing` functor

module Data.Lens.Internal.Focusing
  ( Focusing (..)
  , runFocusing
  ) where

import Prelude
import Data.Monoid (Monoid)
import Data.Tuple (Tuple ())

-- | The functor used to zoom into `StateT`.
newtype Focusing m s a = Focusing (m (Tuple s a))

runFocusing :: forall m s a. Focusing m s a -> m (Tuple s a)
runFocusing (Focusing r) = r

instance focusingFunctor :: (Functor m) => Functor (Focusing m s) where
  map f (Focusing r) = Focusing (map (map f) r)

instance focusingApply :: (Apply m, Semigroup s) => Apply (Focusing m s) where
  apply (Focusing rf) (Focusing rx) = Focusing (map (<*>) rf <*> rx)

instance focusingApplicative :: (Applicative m, Monoid s) => Applicative (Focusing m s) where
  pure = Focusing <<< pure <<< pure
