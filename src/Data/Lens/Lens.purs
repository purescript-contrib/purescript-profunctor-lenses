-- | This module defines functions for working with lenses.

module Data.Lens.Lens
  ( lens
  , lens'
  , withLens
  , cloneLens
  , module Data.Lens.Types
  ) where

import Prelude (id)

import Data.Profunctor (dimap)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))

import Data.Lens.Internal.Shop (Shop(..))
import Data.Lens.Types (Lens(), LensP(), ALens(), ALensP())

lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

-- | Create a `Lens` from a getter/setter pair.
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b

withLens :: forall s t a b r. ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens l f = case l (Shop id \_ b -> b) of Shop x y -> f x y

cloneLens :: forall s t a b. ALens s t a b -> Lens s t a b
cloneLens l = withLens l \x y p -> lens x y p
