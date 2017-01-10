-- | This module defines functions for working with lenses.
module Data.Lens.Lens
  ( lens
  , lens'
  , withLens
  , cloneLens
  , ilens
  , ilens'
  , withIndexedLens
  , cloneIndexedLens
  , module Data.Lens.Types
  ) where

import Prelude

import Data.Lens.Internal.Shop (Shop(..))
import Data.Lens.Types (Lens, Lens', ALens, ALens')
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))

lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

-- | Create a `Lens` from a getter/setter pair.
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b

withLens :: forall s t a b r. ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens l f = case l (Shop id \_ b -> b) of Shop x y -> f x y

cloneLens :: forall s t a b. ALens s t a b -> Lens s t a b
cloneLens l = withLens l \x y p -> lens x y p


ilens' :: forall p i s t a b.
  (s -> Tuple (Tuple i a) (b -> t)) -> IndexedLens i s t a b
ilens' to pab = dimap to (\(Tuple b f) -> f b) (first ((un Indexed) pab))

-- create an `IndexedLens` from a getter/setter pair.
ilens :: forall p i s t a b.
  (s -> Tuple i a) -> (s -> b -> t) -> IndexedLens i s t a b
ilens sia sbt = ilens' \s -> Tuple (sia s) \b -> sbt s b

withIndexedLens :: forall i s t a b r.
  (AnIndexedLens i s t a b) -> ((s -> (Tuple i a)) -> (s -> b -> t) -> r) -> r
withIndexedLens l f = case l (Indexed (IndexedShop id \_ b -> b)) of IndexedShop x y -> f x y

cloneIndexedLens :: forall i s t a b. AnIndexedLens i s t a b -> IndexedLens i s t a b
cloneIndexedLens l = withIndexedLens l \x y p -> ilens x y p
