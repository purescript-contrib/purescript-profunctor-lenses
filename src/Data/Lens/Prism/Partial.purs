module Data.Lens.Prism.Partial
  ( unsafeViewPrism, (^?!)
  , unsafeIndexedFold, (^@?!)
  )
  where

import Prelude

import Data.Lens.Fold (Fold, ifoldMapOf, previewOn)
import Data.Lens.Types (IndexedFold)
import Data.Maybe (fromMaybe', Maybe(..))
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)

unsafeViewPrism :: forall s t a b. Partial => s -> Fold (First a) s t a b -> a
unsafeViewPrism s l = fromMaybe' (crash "(^?!): Empty fold") $ previewOn s l


infixl 8 unsafeViewPrism as ^?!

type KV k v = { index :: k, value :: v}

unsafeIndexedFold
  :: forall i s t a b. Partial
  => s
  -> IndexedFold ((First (KV i a))) i s t a b
  -> (KV i a)
unsafeIndexedFold s l =fromMaybe' (crash "(^@?!): empty Fold")
                         $ unwrap
                           $ ifoldMapOf l (\i a -> First $ Just { index : i, value : a}) s

infixl 8 unsafeIndexedFold as ^@?!

crash :: forall a. String -> Unit -> a
crash msg _ = unsafeCrashWith msg
