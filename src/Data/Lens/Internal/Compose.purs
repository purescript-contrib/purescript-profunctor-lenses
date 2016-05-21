module Data.Lens.Internal.Compose where

import Prelude

data Compose f g a = Compose (f (g a))

decompose :: forall f g a. Compose f g a -> f (g a)
decompose (Compose x) = x

instance functorCompose :: (Functor f, Functor g) => Functor (Compose f g) where
  map f = Compose <<< map (map f) <<< decompose

instance applyCompose :: (Apply f, Apply g) => Apply (Compose f g) where
  apply (Compose f) (Compose x) = Compose $ apply <$> f <*> x

instance applicativeCompose :: (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose <<< pure <<< pure

