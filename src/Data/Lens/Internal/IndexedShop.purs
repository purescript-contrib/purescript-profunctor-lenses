-- | This module defines the `Shop` profunctor
module Data.Lens.Internal.Shop where

import Prelude

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))

-- | The `IndexedShop` profunctor characterizes an `IndexedLens`.
data IndexedShop i a b s t = IndexedShop (s -> (Tuple i a)) (s -> b -> t)

instance profunctorIndexedShop :: Profunctor (IndexedShop i a b) where
  dimap f g (IndexedShop x y) = IndexedShop (x <<< f) (\s -> g <<< y (f s))

instance strongShop :: Strong (IndexedShop i a b) where
  first (IndexedShop x y) =
    IndexedShop (\(Tuple a _) -> x a) (\(Tuple s c) b -> Tuple (y s b) c)
  second (IndexedShop x y) =
    IndexedShop (\(Tuple _ a) -> x a) (\(Tuple c s) b -> Tuple c (y s b))
