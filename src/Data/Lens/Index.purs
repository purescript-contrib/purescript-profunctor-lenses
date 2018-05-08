module Data.Lens.Index
  ( class Index
  , ix
  ) where

import Prelude

import Data.Array as A
import Data.Identity (Identity)
import Data.Lens.Internal.Wander (wander)
import Data.Lens.Types (Traversal')
import Data.Map as M
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Set as S
import Foreign.Object as FO
import Data.Traversable (traverse)

-- | `Index` is a type class whose instances are optics used when:
-- | 1. The focus element might not be present.
-- | 2. You either cannot or do not want to add new elements or delete existing ones. 
-- |
-- | `Array` is a typical example:
-- |
-- | ```purescript 
-- | preview (ix 1) [0, 1, 2] == Just 1
-- |
-- | set (ix 1) 8888 [0, 1, 2] == [0,8888,2]
-- | ```
-- |
-- | Note the use of `preview` rather `view`. That's because the optic is 
-- | a `Data.Lens.Traversal` tailored to the case where there's a single element
-- | of interest.
-- | 
-- | Another common use is a `Map` that you don't want to either grow or shrink:
-- |
-- | ```purescript 
-- | (set (ix "k") "new" $ Map.singleton "k" "old") == Map.singleton "k" "new"
-- | 
-- | (set (ix "k") "new" $ Map.empty) == Map.empty
-- | ```
-- |
-- | Note the second line: an attempt to `set` a missing focus element
-- | leaves the original whole unchanged.
-- |
-- | If you *do* want to add or delete elements, see `Data.Lens.At`. 

class Index m a b | m -> a, m -> b where
  ix :: a -> Traversal' m b

instance indexArr :: Eq i => Index (i -> a) i a where
  ix i =
    wander \coalg f ->
      coalg (f i) <#> \a j ->
        if i == j then a else f j

instance indexMaybe :: Index (Maybe a) Unit a where
  ix _ = wander traverse

instance indexIdentity :: Index (Identity a) Unit a where
  ix _ = wander traverse

instance indexArray :: Index (Array a) Int a where
  ix n =
    wander \coalg xs ->
      xs A.!! n #
        maybe
          (pure xs)
          (coalg >>> map \x -> fromMaybe xs (A.updateAt n x xs))

instance indexSet :: Ord a => Index (S.Set a) a Unit where
  ix x =
    wander \coalg ->
      pure <<< S.insert x

instance indexMap :: Ord k => Index (M.Map k v) k v where
  ix k =
    wander \coalg m ->
      M.lookup k m #
        maybe
          (pure m)
          (coalg >>> map \v -> M.insert k v m)

instance indexForeignObject :: Index (FO.Object v) String v where
  ix k =
    wander \coalg m ->
      FO.lookup k m #
        maybe
          (pure m)
          (coalg >>> map \v -> FO.insert k v m)
