-- | This module defines the `Wander` type class, which is used to define `Traversal`s.

module Data.Lens.Internal.Wander where

import Prelude (Applicative, (<<<), ($))

import Data.Profunctor.Strong (Strong)
import Data.Profunctor.Choice (Choice)
import Data.Profunctor.Star (Star(..), runStar)
import Data.Identity (Identity(..), runIdentity)

-- | Class for profunctors that support polymorphic traversals.
class (Strong p, Choice p) <= Wander p where
  wander
    :: forall s t a b. (forall f. (Applicative f) => (a -> f b) -> s -> f t)
    -> p a b -> p s t

instance wanderFunction :: Wander Function where
  wander t f s = runIdentity $ t (Identity <<< f) s

instance wanderStar :: (Applicative f) => Wander (Star f) where
  wander t = Star <<< t <<< runStar
