module Data.Lens.Record (prop) where

import Prelude

import Data.Lens (Lens, lens)
import Data.Record (get, set)
import Data.Symbol (class IsSymbol, SProxy)

-- | Construct a (type-changing) lens for a record property, by providing a
-- | proxy for the `Symbol` which corresponds to the property label.
-- |
-- | The lens is polymorphic in the rest of the row of property labels.
-- |
-- | For example:
-- |
-- | ```purescript
-- | prop (SProxy :: SProxy "foo")
-- |   :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
-- | ```
prop
  :: forall l r1 r2 r a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> Lens (Record r1) (Record r2) a b
prop l = lens (get l) (flip (set l))
