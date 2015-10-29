## Module Data.Lens.Index

#### `Index`

``` purescript
class Index m a b where
  ix :: a -> TraversalP m b
```

##### Instances
``` purescript
instance indexArr :: (Eq i) => Index (i -> a) i a
instance indexMaybe :: Index (Maybe a) Unit a
instance indexIdentity :: Index (Identity a) Unit a
instance indexArray :: Index (Array a) Int a
instance indexSet :: (Ord a) => Index (Set a) a Unit
instance indexMap :: (Ord k) => Index (Map k v) k v
instance indexStrMap :: Index (StrMap v) String v
```


