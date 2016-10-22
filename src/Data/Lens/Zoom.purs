-- | This module defines functions for zooming in a state monad.
module Data.Lens.Zoom
  ( zoom
  , module Data.Lens.Types
  ) where

import Prelude (($), (<<<))

import Control.Monad.State.Trans (StateT (..), runStateT)

import Data.Lens.Internal.Focusing (Focusing (..), runFocusing)
import Data.Lens.Types
import Data.Profunctor.Star (Star (..))
import Data.Newtype (unwrap)

-- | Zooms into a substate in a `StateT` transformer.
zoom
  :: forall a s r m
   . OpticP (Star (Focusing m r)) s a
  -> StateT a m r
  -> StateT s m r
zoom p ma =
  StateT $ runFocusing <<< unwrap (p $ Star $ Focusing <<< runStateT ma)
