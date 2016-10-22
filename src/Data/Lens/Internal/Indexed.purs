-- | This module defines the `Indexed` profunctor.
module Data.Lens.Internal.Indexed where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens.Internal.Wander (class Wander, wander)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, lmap, dimap)
import Data.Profunctor.Choice (class Choice, right, left)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Tuple (Tuple(..))

-- | Profunctor used for `IndexedOptic`s.
newtype Indexed p i s t = Indexed (p (Tuple i s) t)

derive instance newtypeIndexed :: Newtype (Indexed p i s t) _

instance profunctorIndexed :: Profunctor p => Profunctor (Indexed p i) where
  dimap f g (Indexed p) = Indexed (dimap (second f) g p)

instance strongIndexed :: Strong p => Strong (Indexed p i) where
  first (Indexed p) =
    Indexed $ lmap (\(Tuple i (Tuple a c)) -> (Tuple (Tuple i a) c)) $ first p
  second (Indexed p) =
    Indexed $ lmap (\(Tuple i (Tuple c a)) -> (Tuple c (Tuple i a))) $ second p

instance choiceIndexed :: Choice p => Choice (Indexed p i) where
  left (Indexed p) =
    Indexed $ lmap (\(Tuple i ac) -> either (Left <<< Tuple i) Right ac) $ left p
  right (Indexed p) =
    Indexed $ lmap (\(Tuple i ac) -> either Left (Right <<< Tuple i) ac) $ right p

instance wanderIndexed :: Wander p => Wander (Indexed p i) where
  wander trav (Indexed p) =
    Indexed $ wander (\ia2fb (Tuple i s) -> trav (ia2fb <<< Tuple i) s) p
