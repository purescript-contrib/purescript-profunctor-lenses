-- | This module defines the `Market` profunctor

module Data.Lens.Internal.Market where

import Prelude
import Data.Either
import Data.Profunctor
import qualified Data.Bifunctor as B
import Data.Profunctor.Choice

-- | The `Market` profunctor characterizes a `Prism`.
data Market a b s t = Market (b -> t) (s -> Either t a)

instance functorMarket :: Functor (Market a b s) where
  map f (Market a b) = Market (f <<< a) (B.lmap f <<< b)

instance profunctorMarket :: Profunctor (Market a b) where
  dimap f g (Market a b) = Market (g <<< a) (B.lmap g <<< b <<< f)

instance choiceMarket :: Choice (Market a b) where
  left (Market x y) = Market
    (Left <<< x) (either (B.lmap Left <<< y) (Left <<< Right))
  right (Market x y) = Market
    (Right <<< x) (either (Left <<< Left) (B.lmap Right <<< y))
