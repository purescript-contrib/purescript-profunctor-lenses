-- | This module defines common lenses and prisms.

module Data.Lens.Common where
--------------------------------------------------------------------------------
import Prelude
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Lens.Types
import Data.Lens.Prism
import Data.Lens.Lens
import Data.Lens.Internal.Void
--------------------------------------------------------------------------------

-- | Prism for the `Nothing` constructor of `Maybe`.
_Nothing :: forall a b. Prism (Maybe a) (Maybe b) Unit Unit
_Nothing = prism (const Nothing) $ maybe (Right unit) (const $ Left Nothing)

-- | Prism for the `Just` constructor of `Maybe`.
_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

-- | Prism for the `Left` constructor of `Either`.
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left <<< Right)

-- | Prism for the `Right` constructor of `Either`.
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left <<< Left) Right

-- | Lens for the first component of a `Tuple`.
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_1 = lens (\(Tuple a _) -> a) \(Tuple _ c) b -> Tuple b c

-- | Lens for the second component of a `Tuple`.
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
_2 = lens (\(Tuple _ a) -> a) \(Tuple c _) b -> Tuple c b

-- | There is a `Unit` in everything.
united :: forall a. LensP a Unit
united = lens (const unit) const

-- | There is everything in `Void`.
devoid :: forall a. LensP Void a
devoid = lens absurd const
