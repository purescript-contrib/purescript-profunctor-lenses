-- | This module defines functions for working with getters.

module Data.Lens.Fold
  ( (^?), (^..)
  , preview, foldOf, foldMapOf, foldrOf, foldlOf, toListOf, has, hasn't
  ) where

import Prelude
import Data.Const
import Data.Maybe
import Data.List
import Data.Maybe.First
import Data.Monoid.Endo
import Data.Monoid.Conj
import Data.Monoid.Disj
import Data.Monoid.Dual
import Data.Profunctor.Star
import Data.Lens.Types
import Data.Lens.Internal.Tagged

infixl 8 ^?
infixl 8 ^..

-- | Previews the first value of a fold, if there is any.
preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
preview p = runFirst <<< foldMapOf p (First <<< Just)

-- | Synonym for `preview`, flipped.
(^?) :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
(^?) s p = preview p s

-- | Folds all foci of a `Fold` to one. Note that this is the same as `view`.
foldOf :: forall s t a b. Fold a s t a b -> s -> a
foldOf p = getConst <<< runStar (p (Star Const))

-- | Maps and then folds all foci of a `Fold`.
foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
foldMapOf p f = getConst <<< runStar (p (Star (Const <<< f)))

-- | Right fold over a `Fold`.
foldrOf :: forall s t a b r. Fold (Endo r) s t a b -> (a -> r -> r) -> r -> s -> r
foldrOf p f r = flip runEndo r <<< foldMapOf p (Endo <<< f)

-- | Left fold over a `Fold`.
foldlOf :: forall s t a b r. Fold (Dual (Endo r)) s t a b -> (r -> a -> r) -> r -> s -> r
foldlOf p f r = flip runEndo r <<< runDual <<< foldMapOf p (Dual <<< Endo <<< flip f)

-- | Collects the foci of a `Fold` into a list.
toListOf :: forall s t a b. Fold (Endo (List a)) s t a b -> s -> List a
toListOf p = foldrOf p (:) Nil

-- | Synonym for `toListOf`, reversed.
(^..) :: forall s t a b. s -> Fold (Endo (List a)) s t a b -> List a
(^..) s p = toListOf p s

-- | Determines whether a `Fold` has at least one focus.
has :: forall s t a b r. (BooleanAlgebra r) => Fold (Disj r) s t a b -> s -> r
has p = runDisj <<< foldMapOf p (const (Disj top))

-- | Determines whether a `Fold` does not have a focus.
hasn't :: forall s t a b r. (BooleanAlgebra r) => Fold (Conj r) s t a b -> s -> r
hasn't p = runConj <<< foldMapOf p (const (Conj bottom))
