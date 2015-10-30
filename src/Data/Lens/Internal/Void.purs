-- | This module defines the empty `Void` type.

module Data.Lens.Internal.Void where

import Prelude (Functor, (<$>))

import Data.Functor.Contravariant (Contravariant, (>$<))

import Unsafe.Coerce (unsafeCoerce)

data Void

absurd :: forall a. Void -> a
absurd = unsafeCoerce

coerce :: forall f a b. (Contravariant f, Functor f) => f a -> f b
coerce a = absurd <$> (absurd >$< a)
