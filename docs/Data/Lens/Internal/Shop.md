## Module Data.Lens.Internal.Shop

This module defines the `Shop` profunctor

#### `Shop`

``` purescript
data Shop a b s t
  = Shop (s -> a) (s -> b -> t)
```

The `Shop` profunctor characterizes a `Lens`.

##### Instances
``` purescript
Profunctor (Shop a b)
Strong (Shop a b)
```


