-- | This module defines functions for working with lenses.

module Data.Lens.Prism where

import Prelude

import Data.Either
import Data.Profunctor.Star
import Data.Const
import Data.Maybe
import Data.Maybe.First
import Data.Lens.Types
import Data.Lens.Internal.Tagged
import Data.Profunctor (dimap, rmap)
import Data.Profunctor.Choice (left)

-- | Create a `Prism` from a constructor/pattern pair.
prism :: forall s t a b. (b -> t) -> (s -> Either a t) -> Prism s t a b
prism to fro pab = dimap fro (either id id) (left (rmap to pab))

-- | Review a value through a `Prism`.
review :: forall s t a b. Prism s t a b -> b -> t
review p = unTagged <<< p <<< Tagged

-- | Previews the value of a `Prism`, if there is any.
preview :: forall s t a b. Prism s t a b -> s -> Maybe a
preview p = runFirst <<< getConst <<< runStar (p (Star (Const <<< pure)))
