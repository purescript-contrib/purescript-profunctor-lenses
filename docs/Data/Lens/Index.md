## Module Data.Lens.Index

#### `Index`

``` purescript
class Index m a b where
  ix :: a -> TraversalP m b
```

##### Instances
``` purescript
(Eq i) => Index (i -> a) i a
Index (Maybe a) Unit a
Index (Identity a) Unit a
Index (Array a) Int a
(Ord a) => Index (Set a) a Unit
(Ord k) => Index (Map k v) k v
Index (StrMap v) String v
```


