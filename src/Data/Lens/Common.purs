-- | This module defines common lenses and prisms.
module Data.Lens.Common
  ( module Data.Lens.Lens.Tuple
  , module Data.Lens.Lens.Unit
  , module Data.Lens.Prism.Either
  , module Data.Lens.Prism.Maybe
  ) where

import Data.Lens.Lens.Tuple (_1, _2, first, second)
import Data.Lens.Lens.Unit (united)
import Data.Lens.Prism.Either (_Left, _Right, left, right)
import Data.Lens.Prism.Maybe (_Just, _Nothing)
