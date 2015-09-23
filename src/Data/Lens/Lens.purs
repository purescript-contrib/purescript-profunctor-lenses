-- | This module defines functions for working with lenses.

module Data.Lens.Lens
  ( lens
  , lens'
  , withLens
  , cloneLens
  ) where

import Prelude

import Data.Tuple
import Data.Lens.Types
import Data.Lens.Internal.Shop
import Data.Profunctor
import Data.Profunctor.Strong

lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

-- | Create a `Lens` from a getter/setter pair.
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b

withLens :: forall s t a b r. ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens l f = case l (Shop id \_ b -> b) of Shop x y -> f x y

cloneLens :: forall s t a b. ALens s t a b -> Lens s t a b
cloneLens l = withLens l \x y p -> lens x y p
