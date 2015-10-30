module Data.Lens.Lens.Tuple where

import Data.Tuple (Tuple(..))

import Data.Lens.Lens (Lens(), lens)

-- | Lens for the first component of a `Tuple`.
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_1 = lens (\(Tuple a _) -> a) \(Tuple _ c) b -> Tuple b c

-- | Lens for the second component of a `Tuple`.
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
_2 = lens (\(Tuple _ a) -> a) \(Tuple c _) b -> Tuple c b
