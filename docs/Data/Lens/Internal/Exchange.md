## Module Data.Lens.Internal.Exchange

This module defines the `Exchange` profunctor

#### `Exchange`

``` purescript
data Exchange a b s t
  = Exchange (s -> a) (b -> t)
```

The `Exchange` profunctor characterizes an `Iso`.

##### Instances
``` purescript
Functor (Exchange a b s)
Profunctor (Exchange a b)
```


