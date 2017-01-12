module Data.Lens.Prism.Partial
  ( unsafeViewPrism, (^?!)
  , unsafeIndexedFold, (^@?!))
  where

import Prelude

import Data.Lens.Fold (Fold, foldrOf, ifoldrOf)
import Data.Lens.Types (IndexedFold)
import Data.Monoid.Endo (Endo)
import Partial.Unsafe (unsafeCrashWith)

unsafeViewPrism :: forall s t a b. Partial => s -> Fold (Endo a) s t a b -> a
unsafeViewPrism s l = foldrOf l const (unsafeCrashWith "^?! empty fold") s

infixl 8 unsafeViewPrism as ^?!

type KV k v = { index :: k, value :: v}

unsafeIndexedFold
  :: forall i s t a b. Partial
  => s
  -> IndexedFold ((Endo (KV i a))) i s t a b
  -> (KV i a)
unsafeIndexedFold s l = ifoldrOf l (\i x _ -> { index : i, value : x})
                                   (unsafeCrashWith "(^@?!): empty Fold")
                                   s
infixl 8 unsafeIndexedFold as ^@?!
