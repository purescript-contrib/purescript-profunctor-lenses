-- | This module defines the `Tagged` profunctor
module Data.Lens.Internal.Tagged where

import Prelude

import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Closed (class Closed)
import Data.Profunctor.Costrong (class Costrong)
import Data.Tuple (Tuple(..))

newtype Tagged a b = Tagged b

derive instance newtypeTagged :: Newtype (Tagged a b) _

derive instance eqTagged :: Eq b => Eq (Tagged a b)
instance eq1Tagged :: Eq1 (Tagged a) where eq1 = eq

derive instance ordTagged :: Ord b => Ord (Tagged a b)
instance ord1Tagged :: Ord1 (Tagged a) where compare1 = compare

instance taggedProfunctor :: Profunctor Tagged where
  dimap _ g (Tagged x) = Tagged (g x)

instance taggedChoice :: Choice Tagged where
  left  (Tagged x) = Tagged (Left x)
  right (Tagged x) = Tagged (Right x)

instance taggedCostrong :: Costrong Tagged where
  unfirst (Tagged (Tuple b _)) = Tagged b
  unsecond (Tagged (Tuple _ c)) = Tagged c

instance taggedClosed :: Closed Tagged where
  closed (Tagged b) = Tagged (const b)
