-- | This module defines functions for working with traversals.

module Data.Lens.Traversal
  ( traversed
  , traverseOf
  , sequenceOf
  , failover
  , elementsOf
  , module ExportTypes
  ) where

import Prelude (Applicative, (<<<), ($), pure, id, (==))

import Control.Alternative (Alternative)
import Control.Plus (empty)

import Data.Monoid.Disj (Disj(..))
import Data.Profunctor.Star (Star(..), runStar)
import Data.Traversable (Traversable, traverse)
import Data.Tuple (Tuple(..), uncurry)

import Data.Lens.Types (Traversal(), TraversalP()) as ExportTypes
import Data.Lens.Types (Traversal(), Optic(), Wander, wander)
import Data.Lens.Types (IndexedTraversal(), IndexedOptic(), Indexed(..))
import Data.Lens.Indexed (iwander, positions, unIndex)

-- | Create a `Traversal` which traverses the elements of a `Traversable` functor.
traversed :: forall t a b. (Traversable t) => Traversal (t a) (t b) a b
traversed = wander traverse

-- | Turn a pure profunctor `Traversal` into a `lens`-like `Traversal`.
traverseOf
  :: forall f s t a b. (Applicative f)
  => Optic (Star f) s t a b -> (a -> f b) -> s -> f t
traverseOf t = runStar <<< t <<< Star

-- | Sequence the foci of a `Traversal`, pulling out an `Applicative` effect.
-- | If you do not need the result, see `sequenceOf_` for `Fold`s.
sequenceOf
  :: forall f s t a. (Applicative f)
  => Optic (Star f) s t (f a) a -> s -> f t
sequenceOf t = traverseOf t id

-- | Tries to map over a `Traversal`; returns `empty` if the traversal did
-- | not have any new focus.
failover
  :: forall f s t a b. (Alternative f)
  => Optic (Star (Tuple (Disj Boolean))) s t a b -> (a -> b) -> s -> f t
failover t f s = case runStar (t $ Star $ Tuple (Disj true) <<< f) s of
  Tuple (Disj true) x  -> pure x
  Tuple (Disj false) _ -> empty

-- | Affine traversal the `n`-th focus of a `Traversal`.
element
  :: forall p s t a. (Wander p)
  => Int -> Traversal s t a a -> Optic p s t a a
element n tr = unIndex $ elementsOf (positions tr) (== n)

-- | Traverse elements of an `IndexedTraversal` whose index satisfy a predicate.
elementsOf
  :: forall p i s t a. (Wander p)
  => IndexedTraversal i s t a a -> (i -> Boolean) -> IndexedOptic p i s t a a
elementsOf tr pr = iwander \f -> runStar $ tr $ Indexed $ Star $ \(Tuple i a) -> if pr i then f i a else pure a

-- | Turn a pure profunctor `IndexedTraversal` into a `lens`-like `IndexedTraversal`.
itraverseOf
  :: forall f i s t a b. (Applicative f)
  => IndexedOptic (Star f) i s t a b -> (i -> a -> f b) -> s -> f t
itraverseOf t f = runStar $ t $ Indexed $ Star $ uncurry f
