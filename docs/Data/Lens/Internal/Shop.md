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
instance profunctorShop :: Profunctor (Shop a b)
instance strongShop :: Strong (Shop a b)
```


