-- | This module defines the `Wander` type class, which is used to define
-- | `Traversal`s.
module Data.Lens.Internal.Wander where

import Prelude

import Data.Identity (Identity(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)
import Data.Newtype (unwrap)

-- | Class for profunctors that support polymorphic traversals.
class (Strong p, Choice p) <= Wander p where
  wander
    :: forall s t a b
     . (forall f. Applicative f => (a -> f b) -> s -> f t)
    -> p a b
    -> p s t

instance wanderFunction :: Wander Function where
  wander t f = unwrap <<< t (Identity <<< f)

instance wanderStar :: (Applicative f) => Wander (Star f) where
  wander t = Star <<< t <<< unwrap
