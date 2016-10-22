-- | This module defines functions for zooming in a state monad.
module Data.Lens.Zoom
  ( zoom
  , module Data.Lens.Types
  ) where

import Prelude ((<<<))

import Control.Monad.State.Trans (StateT (..), runStateT)

import Data.Lens.Internal.Focusing (Focusing(..))
import Data.Lens.Types
import Data.Profunctor.Star (Star(..))
import Data.Newtype (under, underF)

-- | Zooms into a substate in a `StateT` transformer.
zoom
  :: forall a s r m
   . Optic' (Star (Focusing m r)) s a
  -> StateT a m r
  -> StateT s m r
zoom p = StateT <<< underF Focusing (under Star p) <<< runStateT
