# purescript-profunctor-lenses

[![Latest release](http://img.shields.io/bower/v/purescript-profunctor-lenses.svg)](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases)
[![Build Status](https://travis-ci.org/purescript-contrib/purescript-profunctor-lenses.svg)](https://travis-ci.org/purescript-contrib/purescript-profunctor-lenses)
[![Maintainer: paf31](https://img.shields.io/badge/maintainer-paf31-lightgrey.svg)](http://github.com/paf31)

Pure profunctor lenses.

## Installation

```
bower install purescript-profunctor-lenses
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-profunctor-lenses).

You can find an example usage [here](test/Main.purs).

## `Lens` vs `ALens`: a Note on Impredicativity

Sometimes, you may encounter some strange errors to do with types that
_should_ unify but don't. For example:

```purescript
newtype Wrapped = Wrapped String
derive instance newtypeWrapped :: Newtype Wrapped _

-- `_Newtype` is an `Iso'`, so we can specialise to `Lens'`.
good :: Lens' Wrapped String
good = _Newtype

-- So, we should be able to do the same when lifted into a record.
type Example = { lens :: Iso' Wrapped String }


bad :: Example -> Lens' Wrapped String
bad = _.lens

-- Could not match constrained type
--    Profunctor t2 => t2 String String -> t2 Wrapped Wrapped
--  with type
--    p0 String String -> p0 Wrapped Wrapped
```

The subtle issue here relates to a subject called *impredictivaty*. We can make
a smaller reproducible example:

```purescript
{ foo: id } :: { foo :: forall a. a -> a }
```

We can, as we would expect, take `foo` out and cast it to whatever we like:

```purescript
({ foo: id } :: { foo :: forall a. a -> a }).foo
  :: String -> String
```

So, while the original type said that we could pick _any_ `a`, we restricted that
to `String` afterwards. The fact that we could pick _any_ `a`, including the type
itself, is called *impredicativity*.

In some situations, such as the above, the type-checker will cope happily. However, the
troubles usually arrive when you extract a value with **a constraint**. Let's generalise
the `id` function a little further:

```purescript
({ foo: id } :: { foo :: forall k a. Category k => k a a })
  :: String -> String
```

Now, Here, we've generalised our `foo` to work with any `Category`, and not just `(->)`.
However, we've also got ourselves a type error! _What did we do?_

We now have a dictionary to contend with. This introduces several practical problems, most
notably that the compiler's inference becomes much less capable. So, while the `expected`
and `actual` type _appear_ to match, the compiler simply can't figure out how in these
situations.

---

Why does this matter to us? Well, let's take a look at the type of your standard `Lens`:

```
> :t lens
forall s t a b. (s -> a) -> (s -> b -> t) -> (forall p. Strong p => p a b -> p s t)
```

Because optic types are defined by polymorphic and constrained types, all of them have a
constraint on _at least_ `Profunctor`. Of course, this means we run into the case of
impredicativity that the compiler inference can't solve.

The usual way to fix this is to introduce a `newtype` or `data` type _around_ the
constrained type, which gives the compiler something that it can infer. To accommodate
this, the optics prefixed with an `A` are defined in terms of a characterising data type
(e.g. `Lens` is defined in terms of `Shop`). Once your optics are free and extracted, the
original, polymorphic type can be recovered using the `cloneLens` function.
