module Data.Lens.Record (prop) where

import Prelude
import Data.StrMap as S
import Data.Lens (Lens, lens)
import Data.Maybe (fromJust)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

unsafeGet
  :: forall r a
   . String
  -> Record r
  -> a
unsafeGet s = unsafePartial fromJust <<< S.lookup s <<< unsafeCoerce

unsafeSet
  :: forall r1 r2 a
   . String
  -> a
  -> Record r1
  -> Record r2
unsafeSet s a = unsafeCoerce <<< S.insert s a <<< unsafeCoerce

get
  :: forall r r' l a
   . IsSymbol l
  => RowCons l a r' r
  => SProxy l
  -> Record r
  -> a
get l = unsafeGet (reflectSymbol l)

set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> Record r1
  -> Record r2
set l = unsafeSet (reflectSymbol l)

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
