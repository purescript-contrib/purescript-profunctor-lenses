module Data.Lens.Prism.Maybe where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Lens.Prism (Prism, prism)

-- | Prism for the `Nothing` constructor of `Maybe`.
_Nothing :: forall a b. Prism (Maybe a) (Maybe b) Unit Unit
_Nothing = prism (const Nothing) $ maybe (Right unit) (const $ Left Nothing)

-- | Prism for the `Just` constructor of `Maybe`.
_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

-- | Prism for the `Nothing` constructor of `Maybe` in a monadic context.
_NothingM :: forall a b m. Monad m => Prism (Maybe a) (m (Maybe b)) Unit (m Unit)
_NothingM = prism (const $ pure Nothing) (maybe (Right unit) (const $ Left $ pure Nothing))

-- | Prism for the `Just` constructor of `Maybe` in a monadic context.
_JustM :: forall a b m. Monad m => Prism (Maybe a) (m (Maybe b)) a (m b)
_JustM = prism (map Just) (maybe (Left $ pure Nothing) Right)
