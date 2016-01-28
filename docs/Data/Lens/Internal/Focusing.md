## Module Data.Lens.Internal.Focusing

This module defines the `Focusing` functor

#### `Focusing`

``` purescript
newtype Focusing m s a
  = Focusing (m (Tuple s a))
```

The functor used to zoom into `StateT`.

##### Instances
``` purescript
instance focusingFunctor :: (Functor m) => Functor (Focusing m s)
instance focusingApply :: (Apply m, Semigroup s) => Apply (Focusing m s)
instance focusingApplicative :: (Applicative m, Monoid s) => Applicative (Focusing m s)
```

#### `runFocusing`

``` purescript
runFocusing :: forall m s a. Focusing m s a -> m (Tuple s a)
```


