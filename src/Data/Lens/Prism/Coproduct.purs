module Data.Lens.Prism.Coproduct where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)

import Data.Lens.Prism (Prism(), prism)

-- | Prism for the `left` of a `Coproduct`.
_Left :: forall f g h a. Prism (Coproduct f g a) (Coproduct h g a) (f a) (h a)
_Left = prism left $ coproduct Right (Left <<< right)

-- | Prism for the `right` of a `Coproduct`.
_Right :: forall f g h a. Prism (Coproduct f g a) (Coproduct f h a) (g a) (h a)
_Right = prism right $ coproduct (Left <<< left) Right
