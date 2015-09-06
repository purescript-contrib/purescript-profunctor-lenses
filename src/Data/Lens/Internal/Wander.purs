-- | This module defines the `Wander` type class, which is used to define `Traversal`s.

module Data.Lens.Internal.Wander where
    
import Prelude

import Data.Traversable (Traversable)
import Data.Profunctor.Strong (Strong)
import Data.Profunctor.Choice (Choice)

class (Strong p, Choice p) <= Wander p where
  wander :: forall t a b. (Traversable t) => p a b -> p (t a) (t b)
  
instance wanderFunction :: Wander Function where
  wander = map