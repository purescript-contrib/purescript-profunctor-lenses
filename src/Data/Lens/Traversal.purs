-- | This module defines functions for working with traversals.

module Data.Lens.Traversal 
  ( traverse
  ) where
    
import Data.Lens.Types
import Data.Traversable (Traversable)
import Data.Lens.Internal.Wander (wander)

-- | Create a `Traversal` which traverses the elements of a `Traversable` functor.
traverse :: forall t a b. (Traversable t) => Traversal (t a) (t b) a b
traverse = wander

