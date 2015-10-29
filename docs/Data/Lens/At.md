## Module Data.Lens.At

#### `At`

``` purescript
class (Index m a b) <= At m a b where
  at :: a -> LensP m (Maybe b)
```

##### Instances
``` purescript
instance atIdentity :: At (Identity a) Unit a
instance atMaybe :: At (Maybe a) Unit a
instance atSet :: (Ord v) => At (Set v) v Unit
instance atMap :: (Ord k) => At (Map k v) k v
instance atStrMap :: At (StrMap v) String v
```


