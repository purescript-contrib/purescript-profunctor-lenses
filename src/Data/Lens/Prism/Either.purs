module Data.Lens.Prism.Either
  ( _Left
  , _Right
  , _LeftM
  , _RightM
  , module Data.Profunctor.Choice
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Lens.Prism (Prism, prism)
import Data.Profunctor.Choice (left, right)

-- | Prism for the `Left` constructor of `Either`.
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = left

-- | Prism for the `Right` constructor of `Either`.
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
_Right = right

-- | Prism for the `Left` constructor of `Either` in a monadic context.
_LeftM :: forall a b c m. Monad m => Prism (Either a c) (m (Either b c)) a (m b)
_LeftM = prism (map Left) (either Right (Left <<< pure <<< Right))

-- | Prism for the `Right` constructor of `Either` in a monadic context.
_RightM :: forall a b c m. Monad m => Prism (Either c a) (m (Either c b)) a (m b)
_RightM = prism (map Right) (lmap (pure <<< Left))
