module Data.Lens.Prism.Either
  ( _Left
  , _Right
  , _LeftF
  , _RightF
  , module Data.Profunctor.Choice
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens.Prism (Prism, prism)
import Data.Profunctor.Choice (left, right)

-- | Prism for the `Left` constructor of `Either`.
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = left

-- | Prism for the `Right` constructor of `Either`.
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
_Right = right

swap :: forall a b. Either a b -> Either b a
swap = case _ of
  Left a -> Right a
  Right a -> Left a

-- | Prism for the `Left` constructor of `Either` in an applicative context.
_LeftF :: forall a b c f. Functor f => Applicative f => Prism (Either a c) (f (Either b c)) a (f b)
_LeftF = prism (map Left) (lmap (pure <<< Right) <<< swap)

-- | Prism for the `Right` constructor of `Either` in an applicative context.
_RightF :: forall a b c f. Functor f => Applicative f => Prism (Either c a) (f (Either c b)) a (f b)
_RightF = prism (map Right) (lmap (pure <<< Left))
