module Data.Lens.Lens.Unit where

import Prelude

import Data.Lens.Lens (LensP(), lens)

-- | There is a `Unit` in everything.
united :: forall a. LensP a Unit
united = lens (const unit) const
