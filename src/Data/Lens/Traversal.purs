-- | This module defines functions for working with traversals.

module Data.Lens.Traversal 
  ( traverse
  , traverseOf
  ) where
    
import Prelude
    
import Data.Tuple
import Data.Either
import Data.Lens.Types
import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice
import Data.Lens.Internal.Wander
import Data.Traversable (Traversable)
import Data.Lens.Internal.Wander (wander)

-- | Create a `Traversal` which traverses the elements of a `Traversable` functor.
traverse :: forall t a b. (Traversable t) => Traversal (t a) (t b) a b
traverse = wander

newtype Upstar f a b = Upstar (a -> f b)

runUpstar :: forall f a b. Upstar f a b -> a -> f b
runUpstar (Upstar f) = f

instance profunctorUpstar :: (Functor f) => Profunctor (Upstar f) where
  dimap f g (Upstar ft) = Upstar (f >>> ft >>> map g)

instance strongUpstar :: (Functor f) => Strong (Upstar f) where
  first  (Upstar f) = Upstar \(Tuple s x) -> map (`Tuple` x) (f s)
  second (Upstar f) = Upstar \(Tuple x s) -> map (Tuple x) (f s)

instance choiceUpstar :: (Applicative f) => Choice (Upstar f) where
  left  (Upstar f) = Upstar $ either (map Left <<< f) (pure <<< Right)
  right (Upstar f) = Upstar $ either (pure <<< Left) (map Right <<< f)

instance wanderUpstar :: (Applicative f) => Wander (Upstar f) where
  wander (Upstar f) = Upstar (Data.Traversable.traverse f)

-- | Turn a pure profunctor `Traversal` into a `lens`-like `Traversal`.
traverseOf :: forall f s t a b. (Applicative f) => Traversal s t a b -> (a -> f b) -> s -> f t
traverseOf t f = runUpstar (t (Upstar f))