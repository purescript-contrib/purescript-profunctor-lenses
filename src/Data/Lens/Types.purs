-- | This module defines types for working with lenses.
module Data.Lens.Types
  ( module Data.Lens.Types
  , module Data.Lens.Internal.Exchange
  , module Data.Lens.Internal.Market
  , module Data.Lens.Internal.Shop
  , module Data.Lens.Internal.Tagged
  , module Data.Lens.Internal.Forget
  , module Data.Lens.Internal.Grating
  , module Data.Lens.Internal.Wander
  , module Data.Lens.Internal.Re
  , module Data.Lens.Internal.Indexed
  ) where

import Data.Tuple
import Data.Lens.Internal.Exchange (Exchange(..))
import Data.Lens.Internal.Forget (Forget(..))
import Data.Lens.Internal.Grating (Grating)
import Data.Lens.Internal.Indexed (Indexed(..))
import Data.Lens.Internal.Market (Market(..))
import Data.Lens.Internal.Re (Re(..))
import Data.Lens.Internal.Shop (Shop(..))
import Data.Lens.Internal.Tagged (Tagged(..))
import Data.Lens.Internal.Wander (class Wander, wander)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Closed (class Closed)
import Data.Profunctor.Strong (class Strong)

-- | Given a type whose "focus element" always exists,
-- | a lens provides a convenient way to view, set, and transform
-- | that element. 
-- | 
-- | For example, `_2` is a tuple-specific `Lens` available from `Data.Lens`, so:
-- | ```purescript
-- | over _2 String.length $ Tuple "ignore" "four" == Tuple "ignore" 4
-- | ```
-- | Note the result has a different type than the original tuple.
-- | That is, the four `Lens` type variables have been narrowed to:
-- |
-- | * `s` is `Tuple String String`
-- | * `t` is `Tuple String Int`
-- | * `a` is `String`
-- | * `b` is `Int`
-- | 
-- | See `Data.Lens.Getter` and `Data.Lens.Setter` for functions and operators
-- | frequently used with lenses.


type Lens s t a b = forall p. Strong p => Optic p s t a b

-- | `Lens'` is a specialization of `Lens`. An optic of type `Lens'`
-- | can change only the value of its focus,
-- | not its type. As an example, consider the `Lens` `_2`, which has this type:
-- |
-- | ```purescript
-- | _2 :: forall s t a b. Lens (Tuple s a) (Tuple t b) a b 
-- | ```
-- |
-- | `_2` can produce a `Tuple Int String` from a `Tuple Int Int`:
-- |
-- | ```purescript
-- | set _2 "NEW" (Tuple 1 2) == (Tuple 1 "NEW")
-- | ```
-- |
-- | If we specialize `_2`'s type with `Lens'`, the following will not
-- | type check:
-- |
-- | ```purescript
-- | set (_2 :: Lens' (Tuple Int Int) Int) "NEW" (Tuple 1 2)
-- |            ^^^^^^^^^^^^^^^^^^^^^^^^^
-- | ```
-- |
-- | See `Data.Lens.Getter` and `Data.Lens.Setter` for functions and operators
-- | frequently used with lenses.

type Lens' s a = Lens s s a a

-- | A prism.
type Prism s t a b = forall p. Choice p => Optic p s t a b
type Prism' s a = Prism s s a a

-- | A generalized isomorphism.
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
type Iso' s a = Iso s s a a

-- | A traversal.
type Traversal s t a b = forall p. Wander p => Optic p s t a b
type Traversal' s a = Traversal s s a a




-- | A general-purpose Data.Lens.
type Optic p s t a b = p a b -> p s t
type Optic' p s a = Optic p s s a a

type AnIso s t a b = Optic (Exchange a b) s t a b
type AnIso' s a = AnIso s s a a

type ALens s t a b = Optic (Shop a b) s t a b
type ALens' s a = ALens s s a a

-- | An indexed lens.
type IndexedLens i s t a b = forall p. Strong p => IndexedOptic p i s t a b
type IndexedLens' i s a = IndexedLens i s s a a

type AnIndexedLens i s t a b = IndexedOptic (Shop (Tuple i a) b) i s t a b
type AnIndexedLens' i s a = AnIndexedLens i s s a a

type APrism s t a b = Optic (Market a b) s t a b
type APrism' s a = APrism s s a a

-- | A grate (http://r6research.livejournal.com/28050.html)
type Grate s t a b = forall p. Closed p => Optic p s t a b
type Grate' s a = Grate s s a a

type AGrate s t a b = Optic (Grating a b) s t a b
type AGrate' s a = AGrate s s a a

-- | A getter.
type Getter s t a b = Fold a s t a b
type Getter' s a = Getter s s a a

-- | A setter.
type Setter s t a b = Optic Function s t a b
type Setter' s a = Setter s s a a

-- | A review.
type Review s t a b = Optic Tagged s t a b
type Review' s a = Review s s a a

-- | A fold.
type Fold r s t a b = Optic (Forget r) s t a b
type Fold' r s a = Fold r s s a a

-- | An indexed optic.
type IndexedOptic p i s t a b = Indexed p i a b -> p s t
type IndexedOptic' p i s a = IndexedOptic p i s s a a

-- | An indexed traversal.
type IndexedTraversal i s t a b = forall p. Wander p => IndexedOptic p i s t a b
type IndexedTraversal' i s a = IndexedTraversal i s s a a

-- | An indexed fold.
type IndexedFold r i s t a b = IndexedOptic (Forget r) i s t a b
type IndexedFold' r i s a = IndexedFold r i s s a a

-- | An indexed getter.
type IndexedGetter i s t a b = IndexedFold a i s t a b
type IndexedGetter' i s a = IndexedGetter i s s a a

-- | An indexed setter.
type IndexedSetter i s t a b = IndexedOptic Function i s t a b
type IndexedSetter' i s a = IndexedSetter i s s a a
