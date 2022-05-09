module Data.Lens.Lens.Maybe where

import Prelude

import Data.Lens.Lens (Lens', lens)
import Data.Maybe (Maybe, fromMaybe)

-- | Feeling lucky? Zoom in on a value if it matches a predicate.
-- | If not, oh well, we use the original.
-- | ```purescript
-- | > view (lucky Just) 1
-- | 1
-- | > over (lucky Just <<< _Just) (add 2) 1
-- | 3
-- | > over (lucky Nothing <<< _Just) (add 2) 1
-- | 1
-- | ```
lucky :: forall a. (a -> Maybe a) -> Lens' a (Maybe a)
lucky f = lens f fromMaybe
