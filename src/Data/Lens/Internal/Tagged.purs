-- | This module defines the `Tagged` profunctor
module Data.Lens.Internal.Tagged where

import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Costrong (class Costrong)
import Data.Tuple (Tuple(..))

newtype Tagged a b = Tagged b

derive instance newtypeTagged :: Newtype (Tagged a b) _

instance taggedProfunctor :: Profunctor Tagged where
  dimap _ g (Tagged x) = Tagged (g x)

instance taggedChoice :: Choice Tagged where
  left  (Tagged x) = Tagged (Left x)
  right (Tagged x) = Tagged (Right x)

instance taggedCostrong :: Costrong Tagged where
  unfirst (Tagged (Tuple b _)) = Tagged b
  unsecond (Tagged (Tuple _ c)) = Tagged c
