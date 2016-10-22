module Data.Lens.Iso.Product where

import Data.Functor.Product (Product(..))
import Data.Tuple (Tuple)
import Data.Newtype (unwrap)

import Data.Lens.Iso (Iso, iso)

_Product
  :: forall f g h i a b
   . Iso
      (Product f g a)
      (Product h i b)
      (Tuple (f a) (g a))
      (Tuple (h b) (i b))
_Product = iso unwrap Product
