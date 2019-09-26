module Data.Lens.Internal.Indexable where

import Prelude

import Data.Lens.Internal.Forget (Forget)
import Data.Newtype (unwrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Lens.Internal.Indexed (Indexed)
import Data.Tuple (Tuple(..), snd)
import Type.Equality (class TypeEquals)
import Type.Equality as TE

class Indexable i p q | p -> q where
  indexed :: forall a b. p a b -> q (Tuple i a) b

instance indexableFunction :: Indexable i (->) (->) where
  indexed = indexedDefault

instance indexableForget :: Indexable i (Forget r) (Forget r) where
  indexed = indexedDefault

instance indexableIndexed :: (Profunctor p, TypeEquals i j) => Indexable i (Indexed p j) p where
  indexed = lcmap (\(Tuple x y) -> Tuple (TE.to x) y) <<< unwrap

indexedDefault :: forall p a b i. Profunctor p => p a b -> p (Tuple i a) b
indexedDefault = lcmap snd