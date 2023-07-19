module Data.Lens.Record.VTA (prop) where

import Data.Lens (Lens)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Data.Lens.Record as Data.Lens.Record

-- | Construct a (type-changing) lens for a record property, by providing a
-- | `Symbol` via VTA (visible type application) which corresponds to the
-- | property label.
-- |
-- | The lens is polymorphic in the rest of the row of property labels.
-- |
-- | For example:
-- |
-- | ```purescript
-- | prop @"foo"
-- |   :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
-- | ```
prop
  :: forall @l r1 r2 r a b
   . IsSymbol l
  => Row.Cons l a r r1
  => Row.Cons l b r r2
  => Lens (Record r1) (Record r2) a b
prop = Data.Lens.Record.prop (Proxy @l)
