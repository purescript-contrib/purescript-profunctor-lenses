module Test.Main where

import Prelude (Unit, ($), bind, (<<<), (<*>), (<$>))

import Data.Maybe (Maybe(Just))
import Data.Lens (view, traversed, _1, _2, _Just, lens)
import Data.Lens.Zoom (Traversal, Lens, zoom)
import Data.Tuple (Tuple(Tuple))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Control.Monad.State (evalState, get)

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = lens _.foo (_ { foo = _ })

bar :: forall a b r. Lens { bar :: a | r } { bar :: b | r } a b
bar = lens _.bar (_ { bar = _ })

type Foo a = { foo :: Maybe { bar :: Array a } }

doc :: Foo String
doc = { foo: Just { bar: [ "Hello", " ", "World" ]} }

bars :: forall a b. Traversal (Foo a) (Foo b) a b
bars = foo <<< _Just <<< bar <<< traversed

stateTest :: Tuple Int String
stateTest = evalState go (Tuple 4 ["Foo", "Bar"]) where
  go = Tuple <$> zoom _1 get <*> zoom (_2 <<< traversed) get

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ view bars doc
  print stateTest
