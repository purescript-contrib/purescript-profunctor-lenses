module Data.Lens.Iso.Newtype where

import Data.Lens.Iso (Iso, iso)
import Data.Newtype (class Newtype, wrap, unwrap)

_Newtype :: forall t a s b. Newtype t a => Newtype s b => Iso t s a b
_Newtype = iso unwrap wrap
