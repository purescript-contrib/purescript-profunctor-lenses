-- | This module defines functions for working with isomorphisms.

module Data.Lens.Iso 
  ( iso
  ) where
    
import Prelude
    
import Data.Tuple
import Data.Lens.Types
import Data.Profunctor
import Data.Profunctor.Strong

-- | Create an `Iso` from a pair of morphisms.
iso :: forall s t a b. (s -> a) -> (b -> t) -> Iso s t a b
iso f g pab = dimap f g pab