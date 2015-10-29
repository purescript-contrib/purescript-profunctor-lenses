module Data.Lens.Contains
  ( Contains
  , contains
  ) where

import Prelude

import Data.Set as S
import Data.Tuple

import Data.Lens (LensP(), lens)
import Data.Lens.Index.Class (IndexKey)

class (IndexKey m a) <= Contains m a where
  contains :: a -> LensP m Boolean

instance containsSet :: (Ord k) => Contains (S.Set k) k where
  contains x = lens (S.member x) \xs b -> if b then S.insert x xs else S.delete x xs
