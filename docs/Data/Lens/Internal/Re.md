## Module Data.Lens.Internal.Re

This module defines the `Re` profunctor

#### `Re`

``` purescript
newtype Re p s t a b
  = Re (p b a -> p t s)
```

##### Instances
``` purescript
instance profunctorRe :: (Profunctor p) => Profunctor (Re p s t)
instance choiceRe :: (Choice p) => Cochoice (Re p s t)
instance cochoiceRe :: (Cochoice p) => Choice (Re p s t)
instance strongRe :: (Strong p) => Costrong (Re p s t)
instance costrongRe :: (Costrong p) => Strong (Re p s t)
```

#### `runRe`

``` purescript
runRe :: forall p s t a b. Re p s t a b -> p b a -> p t s
```


