module Data.Lens.Iso.Product where

import Data.Functor.Product (Product(..), runProduct)
import Data.Tuple (Tuple())

import Data.Lens.Iso (Iso(), iso)

_Product :: forall f g h i a b. Iso (Product f g a) (Product h i b) (Tuple (f a) (g a)) (Tuple (h b) (i b))
_Product = iso runProduct Product
