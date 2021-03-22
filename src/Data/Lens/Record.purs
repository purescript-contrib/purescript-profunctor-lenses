module Data.Lens.Record (prop) where

import Prelude
import Data.Lens (Lens, lens)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Record (get, set)

-- | Construct a (type-changing) lens for a record property, by providing a
-- | proxy for the `Symbol` which corresponds to the property label.
-- |
-- | The lens is polymorphic in the rest of the row of property labels.
-- |
-- | For example:
-- |
-- | ```purescript
-- | prop (Proxy :: Proxy "foo")
-- |   :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
-- | ```
prop
  :: forall l r1 r2 r a b proxy
   . IsSymbol l
  => Row.Cons l a r r1
  => Row.Cons l b r r2
  => proxy l
  -> Lens (Record r1) (Record r2) a b
prop l = lens (get l) (flip (set l))

-- | Like `prop`, but for monadic contexts
propM
  :: forall l r1 r2 r a b proxy m
   . IsSymbol l
  => Row.Cons l a r r1
  => Row.Cons l b r r2
  => Monad m
  => proxy l
  -> Lens (Record r1) (m (Record r2)) a (m b)
propM l =
  lens (get l)
    ( \s mb -> do
        b <- mb
        pure $ set l b s
    )
