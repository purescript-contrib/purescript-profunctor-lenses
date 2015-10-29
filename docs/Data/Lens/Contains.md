## Module Data.Lens.Contains

#### `Contains`

``` purescript
class (IndexKey m a) <= Contains m a where
  contains :: a -> LensP m Boolean
```

##### Instances
``` purescript
instance containsSet :: (Ord k) => Contains (Set k) k
```


