-- | This module defines functions for working with lenses.

module Data.Lens.Types where
    
import Prelude

import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice

import Data.Lens.Internal.Wander

-- | A general-purpose optic.
type Optic p s t a b = p a b -> p s t

-- | A generalized isomorphism.
type Iso s t a b = forall p. (Profunctor p) => Optic p s t a b

-- | A lens.
type Lens s t a b = forall p. (Strong p) => Optic p s t a b

-- | A prism.
type Prism s t a b = forall p. (Choice p) => Optic p s t a b

-- | A traversal.
type Traversal s t a b = forall p. (Wander p) => Optic p s t a b