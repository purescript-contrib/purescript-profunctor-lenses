-- | This module defines functions for working with prisms.
module Data.Lens.Prism
  ( prism, prism', review, nearly, only, clonePrism, withPrism, matching
  , is, isn't
  , module ExportTypes
  ) where

import Prelude

import Control.MonadPlus (guard)

import Data.Either (Either(..), either)
import Data.HeytingAlgebra (tt, ff)
import Data.Lens.Types (Prism, Prism', APrism, APrism', Review, Review') as ExportTypes
import Data.Lens.Types (Prism, Prism', APrism, Market(..), Review, Tagged(..))
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (dimap, rmap)
import Data.Profunctor.Choice (right)
import Data.Newtype (under)

-- | Create a `Prism` from a constructor/pattern pair.
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism to fro pab = dimap fro (either id id) (right (rmap to pab))

prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' to fro = prism to (\s -> maybe (Left s) Right (fro s))

-- | Review a value through a `Prism`.
review :: forall s t a b. Review s t a b -> b -> t
review = under Tagged

nearly :: forall a. a -> (a -> Boolean) -> Prism' a Unit
nearly x f = prism' (const x) (guard <<< f)

only :: forall a. Eq a => a -> Prism a a Unit Unit
only x = nearly x (_ == x)

clonePrism :: forall s t a b. APrism s t a b -> Prism s t a b
clonePrism l = withPrism l \x y p -> prism x y p

withPrism :: forall s t a b r. APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism l f = case l (Market id Right) of
  Market g h -> f g h

matching :: forall s t a b. APrism s t a b -> s -> Either t a
matching l = withPrism l \_ f -> f

is :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
is l = either (const ff) (const tt) <<< matching l

isn't :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
isn't l = not <<< is l
