-- | This module defines the `Indexed` profunctor.
module Data.Lens.Internal.Indexed where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens.Internal.Wander (class Wander, wander)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, right, left)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Tuple (Tuple(..))

-- | Profunctor used for `IndexedOptic`s.
newtype Indexed p i s t = Indexed (p (Tuple i s) t)

-- | Unwrap a value of type `Indexed`.
fromIndexed :: forall p i s t. Indexed p i s t -> p (Tuple i s) t
fromIndexed (Indexed p) = p

instance profunctorIndexed :: Profunctor p => Profunctor (Indexed p i) where
  dimap f g = Indexed <<< dimap (second f) g <<< fromIndexed

instance strongIndexed :: Strong p => Strong (Indexed p i) where
  first =
    Indexed
      <<< dimap (\(Tuple i (Tuple a c)) -> (Tuple (Tuple i a) c)) id
      <<< first
      <<< fromIndexed
  second =
    Indexed
      <<< dimap (\(Tuple i (Tuple c a)) -> (Tuple c (Tuple i a))) id
      <<< second
      <<< fromIndexed

instance choiceIndexed :: Choice p => Choice (Indexed p i) where
  left (Indexed p) =
    Indexed $
      dimap
        (\(Tuple i ac) -> either (Left <<< Tuple i) Right ac)
        id
        (left p)
  right (Indexed p) =
    Indexed $
      dimap
        (\(Tuple i ac) -> either Left (Right <<< Tuple i) ac)
        id
        (right p)

instance wanderIndexed :: Wander p => Wander (Indexed p i) where
  wander trav =
    Indexed
      <<< wander (\ia2fb (Tuple i s) -> trav (ia2fb <<< Tuple i) s)
      <<< fromIndexed
