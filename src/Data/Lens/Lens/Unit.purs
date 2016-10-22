module Data.Lens.Lens.Unit where

import Prelude

import Data.Lens.Lens (Lens', lens)

-- | There is a `Unit` in everything.
united :: forall a. Lens' a Unit
united = lens (const unit) const
