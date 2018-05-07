-- | Prisms are used for selecting cases of a type, most often a sum
-- | type. Consider this:
-- |
-- | ```purescript
-- | data Fill -- think of a paint program filling a shape
-- |   = Solid Color
-- |   ...
-- | ```
-- |
-- | A prism that focuses on `Solid` fills would be written like this:
-- |
-- | ```purescript
-- | solidFocus :: Prism' Fill Color
-- | solidFocus = prism' Solid case _ of
-- |   Solid color -> Just color
-- |   _ -> Nothing
-- | ```
-- |
-- | ... and used like this:
-- |
-- | ```purescript
-- | preview solidFocus (Solid Color.white) == Just Color.white
-- | preview solidFocus NoFill == Nothing
-- |
-- | is solidFocus (Solid Color.white) == true
-- | ```
-- |
-- | `review` can be used to go from a `Color` to a `Fill`:
-- |
-- | ```purescript
-- | review solidFocus Color.white == Solid Color.white
-- | ```
-- | 
-- | For more information, see `PrismsForSumTypes` in the
-- | `examples/src` directory.

module Data.Lens.Prism
  ( prism, prism'
  , only, nearly
  , review
  , is, isn't
  , clonePrism, withPrism, matching
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

-- | Create a `Prism` from a constructor and a "focus" function that
-- | produces an `Either`:
-- | 
-- | ```purescript
-- | solidFocus' :: Prism' Fill Color
-- | solidFocus' = prism Solid case _ of
-- |   Solid color -> Right color
-- |   anotherCase -> Left anotherCase
-- | ```
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism to fro pab = dimap fro (either id id) (right (rmap to pab))

-- | Create a `Prism` from a constructor and a "focus" function that
-- | produces an `Maybe`:
-- | 
-- | ```purescript
-- | solidFocus' :: Prism' Fill Color
-- | solidFocus' = prism' Solid case _ of
-- |   Solid color -> Just color
-- |   _ -> Nothing
-- | ```
prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' to fro = prism to (\s -> maybe (Left s) Right (fro s))

-- | Create a prism that focuses on only some of the values of a case,
-- | such as solid colors that are "bright enough":
-- | 
-- | ```purescript
-- | brightSolidFocus :: Prism' Fill Unit
-- | brightSolidFocus = nearly (Solid referenceColor) predicate
-- |   where
-- |     referenceColor = Color.graytone 0.8
-- |     predicate = case _ of
-- |       Solid color ->
-- |         Color.brightness color >= Color.brightness referenceColor
-- |       _ ->
-- |         false
-- |
-- | preview brightSolidFocus (Solid Color.white) == Just unit
-- | preview brightSolidFocus (Solid Color.black) == Nothing
-- | preview brightSolidFocus NoFill              == Nothing
-- |
-- | is      brightSolidFocus (Solid Color.white) == true
-- | review  brightSolidFocus unit                == Color.graytone 0.8
-- | ```


nearly :: forall a. a -> (a -> Boolean) -> Prism' a Unit
nearly x f = prism' (const x) (guard <<< f)

-- | `only` focuses not just on a case, but a specific value of that case.
-- | 
-- | ```purescript
-- | solidWhiteFocus :: Prism' Fill Unit
-- | solidWhiteFocus = only $ Solid Color.white
-- |
-- | is      solidWhiteFocus (Solid Color.white) == true
-- | preview solidWhiteFocus (Solid Color.white) == Just unit
-- | review  solidWhiteFocus unit                == Solid Color.white
-- | ```
only :: forall a. Eq a => a -> Prism a a Unit Unit
only x = nearly x (_ == x)


-- | Create the "whole" corresponding to a specific "part":
-- |
-- | ```purescript
-- | -- solidFocus is a `Prism Fill Color`
-- | review solidFocus Color.white == Solid Color.white
-- | ```
review :: forall s t a b. Review s t a b -> b -> t
review = under Tagged

clonePrism :: forall s t a b. APrism s t a b -> Prism s t a b
clonePrism l = withPrism l \x y p -> prism x y p

withPrism :: forall s t a b r. APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism l f = case l (Market id Right) of
  Market g h -> f g h

matching :: forall s t a b. APrism s t a b -> s -> Either t a
matching l = withPrism l \_ f -> f

--| Would `preview prism` produce a `Just`?
is :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
is l = either (const ff) (const tt) <<< matching l

--| Would `preview prism` produce a `Nothing`?
isn't :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
isn't l = not <<< is l
