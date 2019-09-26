module Data.Lens.Indexed where

import Prelude

import Control.Monad.State (modify, get, evalState)
import Data.Functor.Compose (Compose(..))
import Data.Lens.Internal.Indexable (indexed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Setter ((%~))
import Data.Lens.Types (wander, Optic, IndexedOptic, Indexed(..), Traversal, IndexedTraversal)
import Data.Newtype (unwrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (first)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), curry, fst, snd)

infixr 9 icomposeBoth as <.>
infixr 9 icomposeLeft as <.
infixr 9 icomposeRight as .>

withIndex :: forall p i s t. p (Tuple i s) t -> Indexed p i s t
withIndex = Indexed

icompose 
  :: forall p i s t a b i' k s' t'
   . Profunctor p 
  => (i -> i' -> k) 
  -> IndexedOptic p i s t s' t' 
  -> IndexedOptic (Indexed p i) i' s' t' a b 
  -> IndexedOptic p k s t a b
icompose f l r = l <<< r <<< withIndex <<< withIndex <<< lcmap (\(Tuple i (Tuple j a)) -> Tuple (f i j) a) <<< indexed

icomposeBoth 
  :: forall p i s t a b i' s' t'
   . Profunctor p 
  => IndexedOptic p i s t s' t' 
  -> IndexedOptic (Indexed p i) i' s' t' a b 
  -> IndexedOptic p (Tuple i i') s t a b 
icomposeBoth = icompose Tuple

icomposeLeft 
  :: forall p i s t a b i' s' t'
   . Profunctor p 
  => IndexedOptic p i s t s' t' 
  -> IndexedOptic (Indexed p i) i' s' t' a b 
  -> IndexedOptic p i s t a b 
icomposeLeft = icompose const

icomposeRight 
  :: forall p i s t a b i' s' t'
   . Profunctor p 
  => IndexedOptic p i s t s' t' 
  -> IndexedOptic (Indexed p i) i' s' t' a b 
  -> IndexedOptic p i' s t a b 
icomposeRight = icompose (const identity)

-- | Converts an `IndexedOptic` to an `Optic` by forgetting indices.
unIndex
  :: forall p i s t a b
   . Profunctor p
  => IndexedOptic p i s t a b
  -> Optic p s t a b
unIndex l = l <<< Indexed <<< dimap snd identity

asIndex
  :: forall p i s t a b
   . Profunctor p
  => IndexedOptic p i s t a b
  -> Optic p s t i b
asIndex l = l <<< Indexed <<< dimap fst identity

-- | Remap the index.
reindexed :: forall p i j r a b . Profunctor p =>
             (i -> j) -> (Indexed p i a b -> r) -> Indexed p j a b -> r
reindexed ij = (_ <<< _Newtype %~ lcmap (first ij))

-- | Converts a `lens`-like indexed traversal to an `IndexedTraversal`.
iwander
  :: forall i s t a b
   . (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IndexedTraversal i s t a b
iwander itr = wander (\f s -> itr (curry f) s) <<< unwrap

-- | Traverses over a `TraversableWithIndex` container.
itraversed
  :: forall i t a b
   . TraversableWithIndex i t
  => IndexedTraversal i (t a) (t b) a b
itraversed = iwander traverseWithIndex

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
