module Data.Lens.Iso.Maybe.First (_First) where

import Data.Lens.Iso (Iso(), iso)
import Data.Maybe (Maybe())
import Data.Maybe.First (First(..), runFirst)

_First :: forall a b. Iso (First a) (First b) (Maybe a) (Maybe b)
_First = iso runFirst First
