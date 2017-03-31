-- | This module defines functions for working with isomorphisms.
module Data.Lens.Iso
  ( iso, withIso, cloneIso, re, au, auf, under, non, curried, uncurried, flipped, mapping, dimapping
  , module Data.Lens.Types
  ) where

import Prelude
import Data.Lens.Types (Iso, Iso', AnIso, AnIso', Optic, Exchange(..), Re(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Tuple (Tuple, curry, uncurry)
import Data.Newtype (unwrap)

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
re t = unwrap (t (Re id))

au :: forall s t a b e. AnIso s t a b -> ((b -> t) -> e -> s) -> e -> a
au l = withIso l \sa bt f e -> sa (f bt e)

auf :: forall s t a b e r p. Profunctor p => AnIso s t a b -> (p r a -> e -> b) -> p r s -> e -> t
auf l = withIso l \sa bt f g e -> bt (f (rmap sa g) e)

under :: forall s t a b. AnIso s t a b -> (t -> s) -> b -> a
under l = withIso l \sa bt ts -> sa <<< ts <<< bt

-- | If `a1` is obtained from `a` by removing a single value, then
-- | `Maybe a1` is isomorphic to `a`.
non :: forall a. Eq a => a -> Iso' (Maybe a) a
non def = iso (fromMaybe def) g
  where g a | a == def  = Nothing
            | otherwise = Just a

curried :: forall a b c d e f. Iso (Tuple a b -> c) (Tuple d e -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry

uncurried :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (Tuple a b -> c) (Tuple d e -> f)
uncurried = iso uncurry curry

flipped :: forall a b c d e f. Iso (a -> b -> c) (d -> e -> f) (b -> a -> c) (e -> d -> f)
flipped = iso flip flip

mapping
  :: forall s t a b f g
   . Functor f
  => Functor g
  => AnIso s t a b
  -> Iso (f s) (g t) (f a) (g b)
mapping l = withIso l \sa bt -> iso (map sa) (map bt)

dimapping
  :: forall s ss t tt a aa b bb p q
   . Profunctor p
  => Profunctor q
  => AnIso s t a b
  -> AnIso ss tt aa bb
  -> Iso (p a ss) (q b tt) (p s aa) (q t bb)
dimapping f g = withIso f \sa bt -> withIso g \ssaa bbtt -> iso (dimap sa ssaa) (dimap bt bbtt)
