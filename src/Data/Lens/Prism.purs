-- | This module defines functions for working with lenses.

module Data.Lens.Prism where
    
import Prelude
    
import Data.Either
import Data.Lens.Types
import Data.Profunctor (dimap, rmap)
import Data.Profunctor.Choice (left)

-- | Create a `Prism` from a constructor/pattern pair.
prism :: forall s t a b. (b -> t) -> (s -> Either a t) -> Prism s t a b
prism to fro pab = dimap fro (either id id) (left (rmap to pab))

