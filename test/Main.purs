module Test.Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Lens
import Data.Tuple
import Data.Traversable (Traversable)

import Control.Monad.Eff.Console

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = lens _.foo (_ { foo = _ })

bar :: forall a b r. Lens { bar :: a | r } { bar :: b | r } a b
bar = lens _.bar (_ { bar = _ })

type Foo a = { foo :: Maybe { bar :: Array a } }

doc :: Foo String
doc = { foo: Just { bar: [ "Hello", " ", "World" ]} }

bars :: forall a b. Traversal (Foo a) (Foo b) a b
bars = foo <<< _Just <<< bar <<< traversed

main = print $ view bars doc
