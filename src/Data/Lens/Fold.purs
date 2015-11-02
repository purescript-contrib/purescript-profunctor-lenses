-- | This module defines functions for working with folds.

module Data.Lens.Fold
  ( (^?), (^..)
  , preview, foldOf, foldMapOf, foldrOf, foldlOf, toListOf, firstOf, lastOf
  , maximumOf, minimumOf, allOf, anyOf, andOf, orOf, elemOf, notElemOf, sumOf
  , productOf, lengthOf, findOf, sequenceOf_, has, hasn't, replicated, filtered
  , module ExportTypes
  ) where

import Prelude

import Control.Apply ((*>))

import Data.Const (Const(..), getConst)
import Data.Either (Either(..), either)
import Data.Foldable (Foldable, foldr)
import Data.Functor.Contravariant (Contravariant)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..), runFirst)
import Data.Maybe.Last (Last(..), runLast)
import Data.Monoid.Additive (Additive(..), runAdditive)
import Data.Monoid.Conj (Conj(..), runConj)
import Data.Monoid.Disj (Disj(..), runDisj)
import Data.Monoid.Dual (Dual(..), runDual)
import Data.Monoid.Endo (Endo(..), runEndo)
import Data.Monoid.Multiplicative (Multiplicative(..), runMultiplicative)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (Choice, right)
import Data.Profunctor.Star (Star(..), runStar)
import Data.Tuple (Tuple(..))

import Data.Lens.Internal.Void (coerce)
import Data.Lens.Types (Fold(), FoldP()) as ExportTypes
import Data.Lens.Types (Optic(), OpticP(), Fold())

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

-- | Whether all foci of a `Fold` satisfy a predicate.
allOf :: forall s t a b r. (BooleanAlgebra r) => Fold (Conj r) s t a b -> (a -> r) -> s -> r
allOf p f = runConj <<< foldMapOf p (Conj <<< f)

-- | Whether any focus of a `Fold` satisfies a predicate.
anyOf :: forall s t a b r. (BooleanAlgebra r) => Fold (Disj r) s t a b -> (a -> r) -> s -> r
anyOf p f = runDisj <<< foldMapOf p (Disj <<< f)

-- | The conjunction of all foci of a `Fold`.
andOf :: forall s t a b. (BooleanAlgebra a) => Fold (Conj a) s t a b -> s -> a
andOf p = allOf p id

-- | The disjunction of all foci of a `Fold`.
orOf :: forall s t a b. (BooleanAlgebra a) => Fold (Disj a) s t a b -> s -> a
orOf p = anyOf p id

-- | Whether a `Fold` contains a given element.
elemOf :: forall s t a b. (Eq a) => Fold (Disj Boolean) s t a b -> a -> s -> Boolean
elemOf p a = anyOf p (== a)

-- | Whether a `Fold` not contains a given element.
notElemOf :: forall s t a b. (Eq a) => Fold (Conj Boolean) s t a b -> a -> s -> Boolean
notElemOf p a = allOf p (/= a)

-- | The sum of all foci of a `Fold`.
sumOf :: forall s t a b. (Semiring a) => Fold (Additive a) s t a b -> s -> a
sumOf p = runAdditive <<< foldMapOf p Additive

-- | The product of all foci of a `Fold`.
productOf :: forall s t a b. (Semiring a) => Fold (Multiplicative a) s t a b -> s -> a
productOf p = runMultiplicative <<< foldMapOf p Multiplicative

-- | The number of foci of a `Fold`.
lengthOf :: forall s t a b. Fold (Additive Int) s t a b -> s -> Int
lengthOf p = runAdditive <<< foldMapOf p (const $ Additive 1)

-- | The first focus of a `Fold`, if there is any. Synonym for `preview`.
firstOf :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
firstOf p = runFirst <<< foldMapOf p (First <<< Just)

-- | The last focus of a `Fold`, if there is any.
lastOf :: forall s t a b. Fold (Last a) s t a b -> s -> Maybe a
lastOf p = runLast <<< foldMapOf p (Last <<< Just)

-- | The maximum of all foci of a `Fold`, if there is any.
maximumOf :: forall s t a b. (Ord a) => Fold (Endo (Maybe a)) s t a b -> s -> Maybe a
maximumOf p = foldrOf p (\a -> Just <<< maybe a (max a)) Nothing where
  max a b = if a > b then a else b

-- | The minimum of all foci of a `Fold`, if there is any.
minimumOf :: forall s t a b. (Ord a) => Fold (Endo (Maybe a)) s t a b -> s -> Maybe a
minimumOf p = foldrOf p (\a -> Just <<< maybe a (min a)) Nothing where
  min a b = if a > b then a else b

-- | Find the first focus of a `Fold` that satisfies a predicate, if there is any.
findOf :: forall s t a b. Fold (Endo (Maybe a)) s t a b -> (a -> Boolean) -> s -> Maybe a
findOf p f = foldrOf p (\a -> maybe (if f a then Just a else Nothing) Just) Nothing

-- | Sequence the foci of a `Fold`, pulling out an `Applicative`, and ignore
-- | the result. If you need the result, see `sequenceOf` for `Traversal`s.
sequenceOf_ :: forall f s t a b. (Applicative f) => Fold (Endo (f Unit)) s t (f a) b -> s -> f Unit
sequenceOf_ p = flip runEndo (pure unit) <<< foldMapOf p \f -> Endo (f *>)

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

-- | Filters on a predicate.
filtered :: forall p a. (Choice p) => (a -> Boolean) -> OpticP p a a
filtered f = dimap (\x -> if f x then Right x else Left x) (either id id) <<< right

-- | Replicates the elements of a fold.
replicated
  :: forall a b t f. (Applicative f, Contravariant f)
  => Int -> Optic (Star f) a b a t
replicated n p = Star (flip go n <<< runStar p) where
  go x 0 = coerce (pure unit)
  go x n = x *> go x (n - 1)

-- | Folds over a `Foldable` container.
folded
  :: forall f g a b t. (Applicative f, Contravariant f, Foldable g)
  => Optic (Star f) (g a) b a t
folded p = Star $ foldr (\a r -> runStar p a *> r) (coerce $ pure unit)

-- | Builds a `Fold` using an unfold.
unfolded
  :: forall f s t a b. (Applicative f, Contravariant f)
  => (s -> Maybe (Tuple a s)) -> Optic (Star f) s t a b
unfolded f p = Star go where
  go = maybe (coerce $ pure unit) (\(Tuple a s) -> runStar p a *> go s) <<< f
