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
import Data.StrMap as SM
import Data.Traversable (traverse)

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

instance indexStrMap :: Index (SM.StrMap v) String v where
  ix k =
    wander \coalg m ->
      SM.lookup k m #
        maybe
          (pure m)
          (coalg >>> map \v -> SM.insert k v m)
