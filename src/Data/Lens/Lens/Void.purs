module Data.Lens.Lens.Void where

import Prelude (const)

import Data.Lens.Lens (Lens', lens)
import Data.Void (Void, absurd)

-- | There is everything in `Void`.
devoid :: forall a. Lens' Void a
devoid = lens absurd const
