-- | This module re-exports types and functions from other modules.

module Data.Lens 
  ( module Data.Lens.Lens
  , module Data.Lens.Prism
  , module Data.Lens.Traversal
  , module Data.Lens.Types
  ) where
    
import Data.Lens.Lens
import Data.Lens.Prism
import Data.Lens.Traversal
import Data.Lens.Types