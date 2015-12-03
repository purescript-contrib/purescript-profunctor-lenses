module Data.Lens.Iso.Maybe.Last (_Last) where

import Data.Lens.Iso (Iso(), iso)
import Data.Maybe (Maybe())
import Data.Maybe.Last (Last(..), runLast)

_Last :: forall a b. Iso (Last a) (Last b) (Maybe a) (Maybe b)
_Last = iso runLast Last
