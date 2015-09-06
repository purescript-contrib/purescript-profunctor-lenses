-- | This module defines functions for working with traversals.

module Data.Lens.Traversal 
  ( traverse
  , traverseOf
  , over
  , viewAll
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

-- | Apply a function to the foci of a `Traversal`.
over :: forall s t a b. Traversal s t a b -> (a -> b) -> s -> t
over l = l

-- | View the foci of a `Traversal`, combining results in some `Monoid`.
viewAll :: forall s t a b m. (Monoid m) => Traversal s t a b -> (a -> m) -> s -> m
viewAll l f = getConst <<< runStar (l (Star (Const <<< f)))