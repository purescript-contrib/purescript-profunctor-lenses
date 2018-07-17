# purescript-profunctor-lenses

[![Latest release](http://img.shields.io/github/release/purescript-contrib/purescript-profunctor-lenses.svg)](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases)
[![Build status](https://travis-ci.org/purescript-contrib/purescript-profunctor-lenses.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-profunctor-lenses)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-profunctor-lenses/badge)](http://pursuit.purescript.org/packages/purescript-profunctor-lenses/)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-lightgrey.svg)](http://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

Pure profunctor lenses. A mechanism for updating, viewing, and setting
values within nested data structures. As in:

```purescript
> structure = Tuple (Tuple (Tuple "hi!" 3) 2) 1

> import Data.Lens
> _leftmost = _1 <<< _1 <<< _1

> view _leftmost structure
"hi!"

> set _leftmost "Bye!" structure 
(Tuple (Tuple (Tuple "Bye!" 3) 2) 1)

> over _leftmost String.toUpper structure
(Tuple (Tuple (Tuple "HI!" 3) 2) 1)

```


## Installation

```
bower install purescript-profunctor-lenses
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-profunctor-lenses).

You can find examples in the [tests](test/Main.purs) and the [examples](examples/README.md) directory.

There is an [ebook](https://leanpub.com/lenses).

## Contributing

Read the [contribution guidelines](https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/.github/contributing.md) to get started and see helpful related resources.
