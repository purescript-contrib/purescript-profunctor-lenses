module Data.Lens.Indexed where

import Prelude

import Control.Monad.State (modify, get, evalState)

import Data.Functor.Compose (Compose(..))
import Data.Lens.Types (wander, Optic, IndexedOptic, Indexed(..), Traversal, IndexedTraversal)
import Data.Newtype (unwrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (curry, fst, snd)

-- | Converts an `IndexedOptic` to an `Optic` by forgetting indices.
unIndex
  :: forall p i s t a b
   . Profunctor p
  => IndexedOptic p i s t a b
  -> Optic p s t a b
unIndex l = l <<< Indexed <<< dimap snd id

asIndex
  :: forall p i s t a b
   . Profunctor p
  => IndexedOptic p i s t a b
  -> Optic p s t i b
asIndex l = l <<< Indexed <<< dimap fst id

-- | Converts a `lens`-like indexed traversal to an `IndexedTraversal`.
iwander
  :: forall i s t a b
   . (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IndexedTraversal i s t a b
iwander itr = wander (\f s -> itr (curry f) s) <<< unwrap

-- | Converts a `Traversal` to an `IndexedTraversal` by using the integer
-- | positions as indices.
positions
  :: forall s t a b
   . Traversal s t a b
  -> IndexedTraversal Int s t a b
positions tr =
  iwander \f s ->
    flip evalState 0 $ unwrap $ flip unwrap s $ tr $ Star \a ->
      Compose $ (f <$> get <*> pure a) <* modify (_ + 1)
