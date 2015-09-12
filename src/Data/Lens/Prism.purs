-- | This module defines functions for working with lenses.

module Data.Lens.Prism
  ( prism, prism', review, nearly, only, clonePrism, withPrism, matching
  , is, isn't
  ) where

import Prelude

import Data.Either
import Data.Profunctor.Star
import Data.Const
import Data.Maybe
import Data.Maybe.First
import Data.Lens.Types
import Data.Lens.Internal.Tagged
import Data.Lens.Internal.Market
import Control.MonadPlus
import Data.Profunctor (dimap, rmap)
import Data.Profunctor.Choice

-- | Create a `Prism` from a constructor/pattern pair.
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism to fro pab = dimap fro (either id id) (right (rmap to pab))

prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> PrismP s a
prism' to fro = prism to (\s -> maybe (Left s) Right (fro s))

-- | Review a value through a `Prism`.
review :: forall s t a b. Review s t a b -> b -> t
review p = unTagged <<< p <<< Tagged

nearly :: forall a. a -> (a -> Boolean) -> PrismP a Unit
nearly x f = prism' (const x) (guard <<< f)

only :: forall a. (Eq a) => a -> Prism a a Unit Unit
only x = nearly x (== x)

clonePrism :: forall s t a b. APrism s t a b -> Prism s t a b
clonePrism l = withPrism l go where
  -- the type checker doesn't like `prism` for `go`...
  go to fro pab = dimap fro (either id id) (right (rmap to pab))

withPrism :: forall s t a b r. APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism l f = case l (Market id Right) of
  Market g h -> f g h

matching :: forall s t a b. APrism s t a b -> s -> Either t a
matching l = withPrism l \_ f -> f

is :: forall s t a b r. (BooleanAlgebra r) => APrism s t a b -> s -> r
is l = either (const bottom) (const top) <<< matching l

isn't :: forall s t a b r. (BooleanAlgebra r) => APrism s t a b -> s -> r
isn't l = not <<< is l
