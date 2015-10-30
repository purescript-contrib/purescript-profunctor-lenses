module Data.Lens.Lens.Product where

import Data.Functor.Product (Product(..))
import Data.Tuple (Tuple(..))

import Data.Lens.Lens (Lens(), lens)

-- | Lens for the first component of a `Product`.
_1 :: forall f g h a. Lens (Product f g a) (Product h g a) (f a) (h a)
_1 = lens (\(Product (Tuple a _)) -> a) \(Product (Tuple _ c)) b -> Product (Tuple b c)

-- | Lens for the second component of a `Product`.
_2 :: forall f g h a. Lens (Product f g a) (Product f h a) (g a) (h a)
_2 = lens (\(Product (Tuple _ a)) -> a) \(Product (Tuple c _)) b -> Product (Tuple c b)
