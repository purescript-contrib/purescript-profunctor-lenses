module Data.Lens.Index
  ( class Index
  , ix
  ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Identity (Identity)
import Data.Lens (Prism', _1, _2, _Just, lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (AffineTraversal')
import Data.Lens.Internal.Wander (wander)
import Data.Lens.Types (Traversal')
import Data.List (List(..), (:))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.StrMap as SM
import Data.Tuple.Nested (Tuple3, tuple3, uncurry3)
import Data.Traversable (traverse)
import Foreign.Object as FO

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
  ix :: a -> AffineTraversal' m b

instance indexFn :: Eq i => Index (i -> a) i a where
  ix i = lens (\f -> f i) \f a j -> if i == j then a else f j

instance indexMaybe :: Index (Maybe a) Unit a where
  ix _ = _Just

instance indexIdentity :: Index (Identity a) Unit a where
  ix _ = _Newtype

instance indexArray :: Index (Array a) Int a where
  ix n = separateIndex <<< _2 <<< _1
    where
      separateIndex :: Prism' (Array a) (Tuple3 (Array a) a (Array a))
      separateIndex = prism'
        (uncurry3 \l e r -> l <> [e] <> r)
        \as -> A.index as n <#> \e -> tuple3
          (A.take n as) e (A.drop (n+1) as)

instance indexNonEmptyArray :: Index (NEA.NonEmptyArray a) Int a where
  ix n =
    wander \coalg xs ->
      xs NEA.!! n #
        maybe
          (pure xs)
          (coalg >>> map \x -> fromMaybe xs (NEA.updateAt n x xs))

instance indexList :: Index (List a) Int a where
  ix n | n < 0     = wander \_ xs -> pure xs
       | otherwise = wander \coalg xs -> go xs n coalg where
    go :: forall f. Applicative f => List a -> Int -> (a -> f a) -> f (List a)
    go Nil _ _ = pure Nil
    go (a:as) 0 coalg = coalg a <#> (_:as)
    go (a:as) i coalg = (a:_) <$> (go as (i - 1) coalg)

instance indexSet :: Ord a => Index (S.Set a) a Unit where
  ix x = lens get (flip update) <<< _Just
    where
      get xs =
        if S.member x xs
           then Just unit
           else Nothing
      update Nothing = S.delete x
      update (Just _) = S.insert x

instance indexMap :: Ord k => Index (M.Map k v) k v where
  ix k = _Just >>>
    lens (M.lookup k) \m ->
      maybe (M.delete k m) \v -> M.insert k v m

instance indexStrMap :: Index (SM.StrMap v) String v where
  ix k = _Just >>>
    lens (SM.lookup k) \m ->
      maybe (SM.delete k m) \ v -> SM.insert k v m

instance indexForeignObject :: Index (FO.Object v) String v where
  ix k =
    wander \coalg m ->
      FO.lookup k m #
        maybe
          (pure m)
          (coalg >>> map \v -> FO.insert k v m)
