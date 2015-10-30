module Data.Lens.Prism.Either where

import Prelude

import Data.Either (Either(..), either)

import Data.Lens.Prism (Prism(), prism)

-- | Prism for the `Left` constructor of `Either`.
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left <<< Right)

-- | Prism for the `Right` constructor of `Either`.
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left <<< Left) Right
