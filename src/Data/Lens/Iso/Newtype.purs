module Data.Lens.Iso.Newtype where

import Data.Lens.Iso (Iso, Iso', iso)
import Data.Newtype (class Newtype, wrap, unwrap)

-- | An Iso between a newtype and its inner type.
-- | Supports switching between different types that have instances of the
-- | Newtype type class.
-- | If you don't need to change types, you may have a better experience with
-- | type inference if you use `simple _Newtype`.
_Newtype :: forall t a s b. Newtype t a => Newtype s b => Iso t s a b
_Newtype = iso unwrap wrap

-- | A variant of `_Newtype` which takes the constructor as an argument
-- | and infers its inverse.
-- |
-- | This is useful as an aid to type inference in certain situations.
unto :: forall n o. Newtype n o => (o -> n) -> Iso' n o
unto _ = _Newtype
