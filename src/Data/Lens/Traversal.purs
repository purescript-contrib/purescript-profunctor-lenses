-- | This module defines functions for working with traversals.

module Data.Lens.Traversal
  ( traverse
  , traverseOf
  ) where

import Prelude

import Data.Const
import Data.Monoid
import Data.Lens.Types
import Data.Profunctor.Star
import Data.Traversable (Traversable)
import Data.Lens.Internal.Wander (wander)

-- | Create a `Traversal` which traverses the elements of a `Traversable` functor.
traverse :: forall t a b. (Traversable t) => Traversal (t a) (t b) a b
traverse = wander

-- | Turn a pure profunctor `Traversal` into a `lens`-like `Traversal`.
traverseOf :: forall f s t a b. (Applicative f) => Traversal s t a b -> (a -> f b) -> s -> f t
traverseOf t f = runStar (t (Star f))
