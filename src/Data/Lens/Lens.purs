-- | This module defines functions for working with lenses.

module Data.Lens.Lens 
  ( lens
  , lens'
  , view
  ) where
    
import Prelude
    
import Data.Tuple
import Data.Lens.Types
import Data.Profunctor
import Data.Profunctor.Strong

-- | Create a `Lens` from an costore-y cothing.
lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

-- | Create a `Lens` from a getter/setter pair.
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b

data View a s t = View (s -> a)

runView :: forall a s t. View a s t -> s -> a
runView (View v) = v

instance profunctorView :: Profunctor (View a) where
  dimap f _ (View v) = View (v <<< f)

instance strongView :: Strong (View a) where
  first  (View v) = View \(Tuple a _) -> v a
  second (View v) = View \(Tuple _ b) -> v b

-- | View the focus of a `Lens`.
view :: forall s t a b. Lens s t a b -> s -> a
view lens s = runView (lens (View id)) s