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
import Data.Tuple(Tuple(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)

unsafeViewPrism :: forall s t a b. Partial => s -> Fold (First a) s t a b -> a
unsafeViewPrism s l = fromMaybe' (crash "unsafeViewPrism: Empty fold") $ previewOn s l

infixl 8 unsafeViewPrism as ^?!


unsafeIndexedFold
  :: forall i s t a b. Partial
  => s
  -> IndexedFold ((First (Tuple i a))) i s t a b
  -> (KV i a)
unsafeIndexedFold s l = fromMaybe' (crash "unsafeIndexedFold: empty Fold")
                          $ unwrap
                           $ ifoldMapOf l (\i a -> First $ Just (Tuple i a)) s

infixl 8 unsafeIndexedFold as ^@?!


crash :: forall a. String -> Unit -> a
crash msg _ = unsafeCrashWith msg
