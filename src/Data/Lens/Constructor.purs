module Data.Lens.Constructor
  ( _Ctor
  , class AsConstructor
  , class AsConstructorRep
  , _CtorRep
  , class ArgumentRep
  , arg
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep as G
import Data.Lens.Iso (Iso, iso, withIso)
import Data.Lens.Prism (Prism, prism)
import Data.Tuple (Tuple(..))
import Type.Prelude (Proxy(..))

class AsConstructor (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  -- | Construct a (type-changing) prism for a data constructor, by providing a
  -- | proxy for the `Symbol` which corresponds to the constructor label.
  -- | Note that you need to derive `Generic` for your data type.
  -- |
  -- | The lens is polymorphic to the rest of the constructors.
  -- |
  -- | For example:
  -- |
  -- | ```purescript
  -- | data Foo a = Foo a | Bar Int | Baz | Multi Int String Boolean
  -- |
  -- | derive instance Generic Foo _
  -- |
  -- | _Foo :: forall a b. Prism (Foo a) (Foo b) a b
  -- | _Foo = _Ctor (Proxy :: _ "Foo")
  -- |
  -- | _Bar :: forall a. Prism' (Foo a) Int
  -- | _Bar = _Ctor (Proxy :: _ "Bar")
  -- |
  -- | _Baz :: forall a. Prism' (Foo a) Unit
  -- | _Baz = _Ctor (Proxy :: _ "Baz")
  -- |
  -- | _Multi :: forall a. Prism' (Foo a) (Tuple Int (Tuple String Boolean))
  -- | _Multi = _Ctor (Proxy :: _ "Multi")
  -- | ```
  _Ctor :: Proxy ctor -> Prism s t a b

instance
  ( AsConstructorRep ctor rep rep' a b
  , G.Generic s rep
  , G.Generic t rep'
  ) =>
  AsConstructor ctor s t a b where
  _Ctor _ = iso G.from G.to <<< _CtorRep (Proxy :: _ ctor)

class AsConstructorRep (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  _CtorRep :: Proxy ctor -> Prism s t a b

instance
  ArgumentRep s t a b =>
  AsConstructorRep ctor (G.Constructor ctor s) (G.Constructor ctor t) a b where
  _CtorRep _ = _Constructor <<< arg

instance
  ArgumentRep s t a b =>
  AsConstructorRep ctor
    (G.Sum (G.Constructor ctor s) r)
    (G.Sum (G.Constructor ctor t) r)
    a
    b where
  _CtorRep _ = _Inl <<< _Constructor <<< arg
else instance
  AsConstructorRep ctor s t a b =>
  AsConstructorRep ctor (G.Sum r s) (G.Sum r t) a b where
  _CtorRep _ = _Inr <<< _CtorRep (Proxy :: _ ctor)

class ArgumentRep s t a b | s -> a, t -> b where
  arg :: Iso s t a b

instance ArgumentRep G.NoArguments G.NoArguments Unit Unit where
  arg = iso (\_ -> unit) (\_ -> G.NoArguments)

instance ArgumentRep (G.Argument a) (G.Argument b) a b where
  arg = _Argument

instance
  ( ArgumentRep s t a b
  , ArgumentRep s' t' a' b'
  ) =>
  ArgumentRep (G.Product s s') (G.Product t t') (Tuple a a') (Tuple b b') where
  arg = withIso arg \from to -> withIso arg \from' to' -> iso
    do \(G.Product s s') -> Tuple (from s) (from' s')
    do \(Tuple a a') -> G.Product (to a) (to' a')

_Inl :: forall a b c. Prism (G.Sum a c) (G.Sum b c) a b
_Inl = prism G.Inl case _ of
  G.Inl a -> Right a
  G.Inr c -> Left (G.Inr c)

_Inr :: forall a b c. Prism (G.Sum c a) (G.Sum c b) a b
_Inr = prism G.Inr case _ of
  G.Inl c -> Left (G.Inl c)
  G.Inr a -> Right a

_Constructor :: forall ctor a b. Iso (G.Constructor ctor a) (G.Constructor ctor b) a b
_Constructor = iso (\(G.Constructor a) -> a) G.Constructor

_Argument :: forall a b. Iso (G.Argument a) (G.Argument b) a b
_Argument = iso (\(G.Argument a) -> a) G.Argument
