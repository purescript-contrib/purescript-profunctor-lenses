module Data.Lens.Indexed where

import Prelude

import Control.Monad.State
import Control.Apply

import Data.Profunctor
import Data.Profunctor.Star
import Data.Profunctor.Strong
import Data.Profunctor.Choice
import Data.Functor.Compose
import Data.Tuple
import Data.Either

import Data.Lens.Types
import Data.Lens.Internal.Indexed
import Data.Lens.Internal.Wander

-- | Converts an `IndexedOptic` to an `Optic` by forgetting indices.
unIndex
  :: forall p i s t a b. (Profunctor p)
  => IndexedOptic p i s t a b -> Optic p s t a b
unIndex l = l <<< Indexed <<< dimap snd id

-- | Converts a `lens`-like indexed traversal to an `IndexedTraversal`.
iwander
  :: forall p i s t a b. (Wander p)
  => (forall f. (Applicative f) => (i -> a -> f b) -> s -> f t)
  -> Indexed p i a b -> p s t
iwander itr = wander (\f s -> itr (curry f) s) <<< fromIndexed

-- | Converts a `Traversal` to an `IndexedTraversal` by using the integer positions as indices.
positions
  :: forall p s t a b. (Wander p)
  => Traversal s t a b -> IndexedOptic p Int s t a b
positions tr = iwander \f s -> flip evalState 0 $ decompose $ flip runStar s $
  tr $ Star \a -> Compose $ (f <$> get <*> pure a) <* modify (+1)
