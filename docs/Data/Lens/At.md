## Module Data.Lens.At

#### `At`

``` purescript
class (Index m a b) <= At m a b where
  at :: a -> LensP m (Maybe b)
```

##### Instances
``` purescript
At (Identity a) Unit a
At (Maybe a) Unit a
(Ord v) => At (Set v) v Unit
(Ord k) => At (Map k v) k v
At (StrMap v) String v
```


