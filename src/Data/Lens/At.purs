module Data.Lens.At
  ( class At
  , at
  ) where


import Prelude

import Data.Identity (Identity(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.StrMap as SM
import Data.Newtype (unwrap)

import Data.Lens (Lens', lens)
import Data.Lens.Index (class Index)


-- | `At` is useful for types like `Data.Map`:
-- | 
-- | * The result of getting an element is a `Maybe a`. 
-- | * New elements can be added.
-- | * Existing elements can be deleted.
-- | 
-- | ```purescript 
-- | > optic = at "key"
-- | > view optic $ Map.singleton "key" "value"
-- | (Just "value")
-- | 
-- | > set optic (Just "NEW") $ Map.singleton "key" "value"
-- | (fromFoldable [(Tuple "key" "NEW")])
-- | 
-- | > set optic Nothing $ Map.singleton "key" "value"
-- | (fromFoldable [])
-- | ```

class Index m a b <= At m a b | m -> a, m -> b where
  at :: a -> Lens' m (Maybe b)

instance atIdentity :: At (Identity a) Unit a where
  at _ = lens (Just <<< unwrap) (flip maybe Identity)

instance atMaybe :: At (Maybe a) Unit a where
  at _ = lens id \_ -> id

instance atSet :: Ord v => At (S.Set v) v Unit where
  at x = lens get (flip update)
    where
      get xs =
        if S.member x xs
           then Just unit
           else Nothing
      update Nothing = S.delete x
      update (Just _) = S.insert x

instance atMap :: Ord k => At (M.Map k v) k v where
  at k =
    lens (M.lookup k) \m ->
      maybe (M.delete k m) \v -> M.insert k v m

instance atStrMap :: At (SM.StrMap v) String v where
  at k =
    lens (SM.lookup k) \m ->
      maybe (SM.delete k m) \ v -> SM.insert k v m
