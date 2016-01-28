## Module Data.Lens.Internal.Forget

#### `Forget`

``` purescript
newtype Forget r a b
  = Forget (a -> r)
```

Profunctor that forgets the `b` value and returns (and accumulates) a
value of type `r`.

`Forget r` is isomorphic to `Star (Const r)`, but can be given a `Cochoice`
instance.

##### Instances
``` purescript
instance profunctorForget :: Profunctor (Forget r)
instance choiceForget :: (Monoid r) => Choice (Forget r)
instance strongForget :: Strong (Forget r)
instance cochoiceForget :: Cochoice (Forget r)
instance wanderForget :: (Monoid r) => Wander (Forget r)
```

#### `runForget`

``` purescript
runForget :: forall r a b. Forget r a b -> a -> r
```

Unwrap a value of type `Forget`.


