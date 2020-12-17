# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

New features:

Bugfixes:

Other improvements:

## [v6.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.3.0) - 2020-02-10

This release includes a number of small but meaningful improvements to the library:

- Makes `indexList` more efficient and adds an `Index` instance for `List` (@cscalfani)
- Adds a re-export for `ATraversal` and `ATraversal'` (@pbrant)
- Adds a new function, `simple`, which can be used to restrict the type of an optic to aid type inference (@LiamGoodacre)
- Adds a new `lensStore` function which can be used to construct a lens when a type appears under every constructor in a sum type (@LiamGoodacre)
- Adds `toArrayOf` and `toArrayOfOn` which behave like the existing `toListOf` and `toListOfOn` optics, but for arrays (@dwhitney)

## [v6.2.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.2.0) - 2019-04-08

- Added `Index` instance for `NonEmptyArray` (@reactormonk)

## [v6.1.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.1.1) - 2019-03-27

- Removed unnecessary `Applicative` constraints from `Traversal` functions.

## [v6.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.1.0) - 2019-03-16

- Added `ATraversal` and `cloneTraversal` (@pbrant)

## [v6.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.0.0) - 2019-03-04

- Updated dependencies for the latest `-foreign-object` and `-record`

## [v5.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v5.0.0) - 2018-10-15

- Make Getters compose and define AGetter/cloneGetter (@LiamGoodacre)

## [v4.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v4.0.0) - 2018-05-25

- Updates for 0.12

## [v3.8.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.8.0) - 2017-11-07

- Added various instances for `Tagged`

## [v3.7.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.7.0) - 2017-09-13

- Add `takeBoth` (@coot)

## [v3.6.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.6.1) - 2017-08-31

- Add explicit `foldable-traversable` dependency

## [v3.6.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.6.0) - 2017-08-13

- Add `Monoid Forget` instance (@joneshf)

## [v3.5.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.5.0) - 2017-08-13

- Add `Closed Tagged` instance (@joneshf)

## [v3.4.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.4.0) - 2017-07-31

- Use `purescript-record` (@coot)

## [v3.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.3.0) - 2017-07-27

- Add `iforOf`, `iforOf_`, `itraversed` and `reindexed` (@Rufflewind)

## [v3.2.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.2.0) - 2017-04-13

- Add `prop` lenses for record fields.

## [v3.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.1.0) - 2017-04-12

- Add `Grate` and corresponding functions.

## [v3.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.0.0) - 2017-04-02

- Updates for 0.11.1

## [v2.5.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.5.0) - 2017-01-16

- Add `unsafeView` and `unsafeIndexedFold` (@boothead)

## [v2.4.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.4.0) - 2017-01-13

- Add indexed lenses (@Roxxik)

## [v2.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.3.0) - 2016-12-20

- Export `traverseOf_` (@Roxxik)

## [v2.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.1.0) - 2016-10-31

- Add `asIndex` (@puffnfresh)

## [v2.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.0.0) - 2016-10-22

- Updated dependencies
- All `P`-named varieties of optics now use `'` intead (`LensP` -> `Lens'`, etc)
- `_Coproduct` and `_Product` isos have been removed as they fall under the new `_Newtype` iso
- Added functional dependencies to `Index` and `At` (@tslawler)

## [v1.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v1.0.0) - 2016-06-02

- Updates for 1.0 core libraries.

## [v1.0.0-rc.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v1.0.0-rc.1) - 2016-05-21

- Release candidate for the PureScript 0.9 compiler.

## [v0.5.4](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.4) - 2016-05-02

- Update bower.json (@hdgarrood)

## [v0.5.3](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.3) - 2016-04-24

- Export `iover` (@nathanfaubion)

## [v0.5.2](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.2) - 2016-04-21

- Added `Wander` instance to `Indexed` (@tslawler)

## [v0.5.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.1) - 2016-04-16

- Export `itraverseOf` and `element` (@zrho)

## [v0.5.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.0) - 2016-02-27

- Convert functions to `Forget`, including `to` (@zrho)

## [v0.4.2](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.4.2) - 2016-01-28

- Add `IndexedTraversal` and friends (@zrho)

## [v0.4.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.4.1) - 2016-01-24

- Fixed behaviour of `Data.Lens.Fold.minimumOf` (@scott-christopher)

## [v0.4.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.4.0) - 2016-01-12

- Add `zoom` and `MonadState` combinators.
- Add `Re` profunctor to turn around isos into isos, lenses into reviews, etc.

By @zrho

## [v0.3.5](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.5) - 2015-11-20

- Removed unused imports

## [v0.3.4](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.4) - 2015-11-07

- Reexport `first`, `second`, `left` and `right`.

## [v0.3.3](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.3) - 2015-11-02

- Removed unused imports

## [v0.3.2](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.2) - 2015-10-30

- Added isomorphisms for `Product`/`Tuple` and `Coproduct`/`Either`
- Added lenses and prisms for `Product` and `Coproduct`
- Split up the existing common predefined lenses and prisms so they can be imported individually by type
- Added more re-exports throughout

## [v0.3.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.1) - 2015-10-29

- Add `At` and `Index` (@jonsterling)

## [v0.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.0) - 2015-09-23

- Generalize `Wander`
- Add standard lenses, traversals and folds

by @zrho

## [v0.2.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.2.0) - 2015-09-13

- Add functions based on `purescript-lens` and `purescript-optic` (@zrho)

## [v0.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.1.0) - 2015-09-06

Initial release
