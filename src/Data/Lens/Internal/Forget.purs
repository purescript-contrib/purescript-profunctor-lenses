module Data.Lens.Internal.Forget where

import Prelude

import Data.Tuple (Tuple (..), fst, snd)
import Data.Either (Either (..), either)
import Data.Monoid (Monoid, mempty)
import Data.Const (Const (..), getConst)
import Data.Profunctor (Profunctor)
import Data.Profunctor.Strong (Strong)
import Data.Profunctor.Choice (Choice)
import Data.Profunctor.Cochoice (Cochoice)

import Data.Lens.Internal.Wander (Wander)

-- | Profunctor that forgets the `b` value and returns (and accumulates) a
-- | value of type `r`.
-- |
-- | `Forget r` is isomorphic to `Star (Const r)`, but can be given a `Cochoice`
-- | instance.
newtype Forget r a b = Forget (a -> r)

-- | Unwrap a value of type `Forget`.
runForget :: forall r a b. Forget r a b -> a -> r
runForget (Forget z) = z

instance profunctorForget :: Profunctor (Forget r) where
  dimap f _ (Forget z) = Forget (z <<< f)

instance choiceForget :: (Monoid r) => Choice (Forget r) where
  left  (Forget z) = Forget (either z mempty)
  right (Forget z) = Forget (either mempty z)

instance strongForget :: Strong (Forget r) where
  first  (Forget z) = Forget (z <<< fst)
  second (Forget z) = Forget (z <<< snd)

instance cochoiceForget :: Cochoice (Forget r) where
  unleft  (Forget z) = Forget (z <<< Left)
  unright (Forget z) = Forget (z <<< Right)

instance wanderForget :: (Monoid r) => Wander (Forget r) where
  wander f (Forget r) = Forget \s -> getConst (f (Const <<< r) s)

-- forall s t a b. (forall f. (Applicative f) => (a -> f b) -> s -> f t)
-- -> p a b -> p s t
