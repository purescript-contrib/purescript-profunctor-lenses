module Test.Main where

import Prelude

import Data.Lens
import Data.Tuple

import Control.Monad.Eff.Console

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = lens _.foo (_ { foo = _ })

bar :: forall a b r. Lens { bar :: a | r } { bar :: b | r } a b
bar = lens _.bar (_ { bar = _ })

main = do
  log $ view (foo <<< bar) { foo: { bar: "Hello, World!"} }
