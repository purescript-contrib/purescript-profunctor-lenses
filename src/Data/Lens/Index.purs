module Data.Lens.Index
  ( class Index
  , ix
  ) where

import Prelude

import Data.Array as A
import Data.Identity (Identity)
import Data.Lens (Prism', _1, _2, _Just, lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (AffineTraversal')
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.StrMap as SM
import Data.Tuple.Nested (Tuple3, tuple3, uncurry3)

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
