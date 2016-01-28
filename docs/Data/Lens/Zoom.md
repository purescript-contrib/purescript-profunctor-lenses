## Module Data.Lens.Zoom

This module defines functions for zooming in a state monad.

#### `zoom`

``` purescript
zoom :: forall a s r m. OpticP (Star (Focusing m r)) s a -> StateT a m r -> StateT s m r
```

Zooms into a substate in a `StateT` transformer.


