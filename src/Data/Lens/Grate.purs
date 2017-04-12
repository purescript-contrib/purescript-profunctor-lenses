-- | This module defines functions for working with grates.
-- |
-- | See <http://r6research.livejournal.com/28050.html>.
module Data.Lens.Grate
  ( grate
  , withGrate
  , cloneGrate
  , cotraversed
  , zipWithOf
  , zipFWithOf
  , collectOf
  , module Data.Lens.Types
  ) where

import Prelude
import Data.Distributive (class Distributive, cotraverse)
import Data.Lens.Internal.Grating (Grating(..))
import Data.Lens.Internal.Zipping (Zipping(..))
import Data.Lens.Types (Grate, Grate', Optic, AGrate)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Profunctor.Closed (closed)
import Data.Profunctor.Costar (Costar(..))
import Data.Profunctor.Star (Star(..))

grate :: forall s t a b. (((s -> a) -> b) -> t) -> Grate s t a b
grate f pab = dimap (#) f (closed pab)

withGrate :: forall s t a b. AGrate s t a b -> ((s -> a) -> b) -> t
withGrate g = unwrap (g (Grating \f -> f id))

cloneGrate :: forall s t a b. AGrate s t a b -> Grate s t a b
cloneGrate g = grate (withGrate g)

cotraversed :: forall f a b. Distributive f => Grate (f a) (f b) a b
cotraversed = grate \f -> cotraverse f id

zipWithOf :: forall s t a b. Optic Zipping s t a b -> (a -> a -> b) -> s -> s -> t
zipWithOf g f = unwrap (g (Zipping f))

zipFWithOf :: forall f s t a b. Optic (Costar f) s t a b -> (f a -> b) -> (f s -> t)
zipFWithOf g f = unwrap (g (Costar f))

collectOf :: forall f s t a b. Optic (Star f) s t a b -> (a -> f b) -> s -> f t
collectOf g f = unwrap (g (Star f))
