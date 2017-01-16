module Test.Main where

import Prelude

import Data.Lens (view, traversed, _1, _2, _Just, _Left, lens)
import Data.Lens.Index (ix)
import Data.Lens.Setter (iover)
import Data.Lens.Lens (ilens, IndexedLens, cloneIndexedLens)
import Data.Lens.Fold ((^?))
import Data.Lens.Fold.Partial ((^?!), (^@?!))
import Data.Lens.Zoom (Traversal, Traversal', Lens, Lens', zoom)
import Data.Tuple  (Tuple(..))
import Data.Maybe  (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.State (evalState, get)
import Partial.Unsafe (unsafePartial)

-- Traversing an array nested within a record
foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = lens _.foo (_ { foo = _ })

bar :: forall a b r. Lens { bar :: a | r } { bar :: b | r } a b
bar = lens _.bar (_ { bar = _ })

type Foo a = { foo :: Maybe { bar :: Array a } }

doc :: Foo String
doc = { foo: Just { bar: [ "Hello", " ", "World" ]} }

bars :: forall a b. Traversal (Foo a) (Foo b) a b
bars = foo <<< _Just <<< bar <<< traversed

-- Get the index of a deeply nested record
type BarRec = { foo :: Array (Either { bar :: Array Int } String) }
data Bar = Bar BarRec

_Bar :: Lens' Bar BarRec
_Bar = lens (\(Bar rec) -> rec) (\_ -> Bar)

doc2 :: Bar
doc2 = Bar { foo: [Left { bar: [ 1, 2, 3 ] }] }

_0Justbar :: Traversal' Bar (Either { bar :: Array Int } String)
_0Justbar = _Bar <<< foo <<< ix 0

_1bars :: Traversal' Bar Int
_1bars = _0Justbar <<< _Left <<< bar <<< ix 1

-- Tests state using zoom
stateTest :: Tuple Int String
stateTest = evalState go (Tuple 4 ["Foo", "Bar"]) where
  go = Tuple <$> zoom _1 get <*> zoom (_2 <<< traversed) get

--test cloning of indexed lenses
cloneTest :: Tuple Int (Tuple Int Int)
cloneTest = iover (cloneIndexedLens i_2) Tuple (Tuple 1 2)

i_2 :: forall a b c. IndexedLens Int (Tuple a b) (Tuple a c) b c
i_2 = ilens (\(Tuple _ b) -> Tuple 0 b) (\(Tuple a _) b -> Tuple a b)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ view bars doc
  logShow $ doc2 ^? _1bars
  logShow $ unsafePartial $ doc2 ^?! _1bars
  logShow $ unsafePartial $ Tuple 0 1 ^@?! i_2
  logShow stateTest
  logShow cloneTest
