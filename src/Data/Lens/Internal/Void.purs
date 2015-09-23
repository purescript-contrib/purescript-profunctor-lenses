-- | This module defines the empty `Void` type.

module Data.Lens.Internal.Void where

import Prelude
import Data.Functor.Contravariant
import Unsafe.Coerce

data Void

absurd :: forall a. Void -> a
absurd = unsafeCoerce

coerce :: forall f a b. (Contravariant f, Functor f) => f a -> f b
coerce a = absurd <$> (absurd >$< a)
