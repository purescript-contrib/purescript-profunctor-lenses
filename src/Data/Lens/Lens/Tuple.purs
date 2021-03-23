module Data.Lens.Lens.Tuple
  ( _1
  , _2
  , _1F
  , _2F
  , module Data.Profunctor.Strong
  ) where

import Prelude

import Data.Lens.Lens (Lens, lens)
import Data.Profunctor.Strong (first, second)
import Data.Tuple (Tuple(..), fst, snd)

-- | Lens for the first component of a `Tuple`.
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_1 = first

-- | Lens for the second component of a `Tuple`.
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
_2 = second

-- | Lens for the first component of a `Tuple` in a monadic context.
_1F :: forall a b c f. Functor f => Lens (Tuple a c) (f (Tuple b c)) a (f b)
_1F =
  lens fst (map <<< flip Tuple <<< snd)

-- | Lens for the second component of a `Tuple` in a monadic context.
_2F :: forall a b c f. Functor f => Lens (Tuple c a) (f (Tuple c b)) a (f b)
_2F =
  lens snd (map <<< Tuple <<< fst)
