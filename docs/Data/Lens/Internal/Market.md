## Module Data.Lens.Internal.Market

This module defines the `Market` profunctor

#### `Market`

``` purescript
data Market a b s t
  = Market (b -> t) (s -> Either t a)
```

The `Market` profunctor characterizes a `Prism`.

##### Instances
``` purescript
instance functorMarket :: Functor (Market a b s)
instance profunctorMarket :: Profunctor (Market a b)
instance choiceMarket :: Choice (Market a b)
```


