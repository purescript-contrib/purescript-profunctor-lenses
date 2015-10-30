module Data.Lens.Lens.Void where

import Prelude (const)

import Data.Lens.Internal.Void (Void(), absurd)

import Data.Lens.Lens (LensP(), lens)

-- | There is everything in `Void`.
devoid :: forall a. LensP Void a
devoid = lens absurd const
