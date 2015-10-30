-- | This module defines the `Tagged` profunctor

module Data.Lens.Internal.Tagged where

import Data.Profunctor (Profunctor)
import Data.Profunctor.Choice (Choice)
import Data.Either (Either(..))

newtype Tagged a b = Tagged b

instance taggedProfunctor :: Profunctor Tagged where
  dimap _ g (Tagged x) = Tagged (g x)

instance taggedChoice :: Choice Tagged where
  left  (Tagged x) = Tagged (Left x)
  right (Tagged x) = Tagged (Right x)

unTagged :: forall a b. Tagged a b -> b
unTagged (Tagged x) = x
