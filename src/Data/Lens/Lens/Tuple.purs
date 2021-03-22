module Data.Lens.Lens.Tuple
  ( _1
  , _2
  , _1M
  , _2M
  , module Data.Profunctor.Strong
  ) where

import Prelude
import Data.Lens.Lens (Lens, lens)
import Data.Profunctor.Strong (first, second)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))

-- | Lens for the first component of a `Tuple`.
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_1 = first

-- | Lens for the second component of a `Tuple`.
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
_2 = second

-- | Lens for the first component of a `Tuple` in a monadic context.
_1M :: forall a b c m. Monad m => Lens (Tuple a c) (m (Tuple b c)) a (m b)
_1M =
  lens fst \(_ /\ b) ma -> do
    a <- ma
    pure $ a /\ b

-- | Lens for the second component of a `Tuple` in a monadic context.
_2M :: forall a b c m. Monad m => Lens (Tuple c a) (m (Tuple c b)) a (m b)
_2M =
  lens snd \(a /\ _) mb -> do
    b <- mb
    pure $ a /\ b
