module Data.Lens.Lens.Unit where

import Prelude

import Data.Lens.Lens (Lens', lens)

-- | There is a `Unit` in everything.
-- | ```purescript
-- | > view united [1,2,3]
-- | Unit
-- | > over united (\a -> a) [1,2,3]
-- | [1 2 3]
-- | ```
united :: forall a. Lens' a Unit
united = lens (const unit) const
