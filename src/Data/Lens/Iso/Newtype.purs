module Data.Lens.Iso.Newtype
  ( _Newtyped
  , _Newtype
  ) where

import Data.Lens.Iso (Iso, Iso', iso)
import Data.Newtype (class Newtype, wrap, unwrap)

-- | An Iso between a newtype and it's inner type.
-- | Fixed to a single type with an instance of the Newtype type class.
_Newtyped :: forall s a. Newtype s a => Iso' s a
_Newtyped = iso unwrap wrap

-- | An Iso between a newtype and it's inner type.
-- | Supports switching between different types that have instances of the
-- | Newtype type class.
-- | If you don't need to change types, you may have a better experience with
-- | type inference if you use _Newtyped instead.
_Newtype :: forall t a s b. Newtype t a => Newtype s b => Iso t s a b
_Newtype = iso unwrap wrap

