module Data.Lens.Prism.Coproduct where

import Prelude ((<<<))

import Data.Functor.Coproduct (Coproduct())

import Data.Lens.Iso.Coproduct (_Coproduct)
import Data.Lens.Prism (Prism())
import Data.Lens.Prism.Either as E

-- | Prism for the `left` of a `Coproduct`.
_Left :: forall f g h a. Prism (Coproduct f g a) (Coproduct h g a) (f a) (h a)
_Left = _Coproduct <<< E._Left

-- | Prism for the `right` of a `Coproduct`.
_Right :: forall f g h a. Prism (Coproduct f g a) (Coproduct f h a) (g a) (h a)
_Right = _Coproduct <<< E._Right
