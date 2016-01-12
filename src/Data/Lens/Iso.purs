-- | This module defines functions for working with isomorphisms.

module Data.Lens.Iso
  ( iso, withIso, cloneIso, re, au, auf, under, curried, uncurried, flipped
  , module Data.Lens.Types
  ) where

import Prelude ((<<<), flip, id)

import Data.Profunctor (Profunctor, dimap, rmap)
import Data.Tuple (Tuple(), curry, uncurry)

import Data.Lens.Types (Iso(), IsoP(), AnIso(), AnIsoP(), Optic(), Exchange(..), Re(..), runRe)

-- | Create an `Iso` from a pair of morphisms.
iso :: forall s t a b. (s -> a) -> (b -> t) -> Iso s t a b
iso f g pab = dimap f g pab

-- | Extracts the pair of morphisms from an isomorphism.
withIso :: forall s t a b r. AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso l f = case l (Exchange id id) of
  Exchange g h -> f g h

-- | Extracts an `Iso` from `AnIso`.
cloneIso :: forall s t a b. AnIso s t a b -> Iso s t a b
cloneIso l = withIso l \x y p -> iso x y p

-- | Reverses an optic.
re :: forall p s t a b. Optic (Re p a b) s t a b -> Optic p b a t s
re t = runRe (t (Re id))

au :: forall s t a b e. AnIso s t a b -> ((b -> t) -> e -> s) -> e -> a
au l = withIso l \sa bt f e -> sa (f bt e)

auf :: forall s t a b e r p. (Profunctor p) => AnIso s t a b -> (p r a -> e -> b) -> p r s -> e -> t
auf l = withIso l \sa bt f g e -> bt (f (rmap sa g) e)

under :: forall s t a b. AnIso s t a b -> (t -> s) -> b -> a
under l = withIso l \sa bt ts -> sa <<< ts <<< bt

curried :: forall a b c d e f. Iso (Tuple a b -> c) (Tuple d e -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry

uncurried :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (Tuple a b -> c) (Tuple d e -> f)
uncurried = iso uncurry curry

flipped :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (b -> a -> c) (e -> d -> f)
flipped = iso flip flip
