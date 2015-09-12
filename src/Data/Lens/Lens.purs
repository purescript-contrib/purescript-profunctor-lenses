-- | This module defines functions for working with lenses.

module Data.Lens.Lens
  ( lens
  , lens'
  ) where

import Prelude

import Data.Tuple
import Data.Lens.Types
import Data.Profunctor
import Data.Profunctor.Strong

lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

-- | Create a `Lens` from a getter/setter pair.
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b
