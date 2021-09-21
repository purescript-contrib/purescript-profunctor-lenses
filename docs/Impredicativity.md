# `Lens` vs `ALens`: a Note on Impredicativity

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

Below, we'll discuss how these problems arise, and what we can do to overcome
them.

## The Long Answer

The subtle issue here relates to a subject called *impredicativity*. We can make
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
to `String` afterwards. In some situations, such as the above, the type-checker
will cope happily. However, the troubles usually arrive when you extract a value
with **a constraint**. Let's generalise the `id` function a little further:

```purescript
({ foo: id } :: { foo :: forall k a. Category k => k a a }).foo
 :: String -> String
```

Now, we've generalised our `foo` to work with any `Category`, and not just
`(->)`. However, we've also got ourselves a type error! _What did we do?_

Let's break this line down into two parts:

1. We extract `foo` out of our record, which has the type signature: `forall k a. Category k => k a a`.
2. We specialise `forall k a. Category k => k a a` to `String -> String`.

Now, step `1` and `2`, independently, seem fine and dandy; they certainly work
if we try it with a function _not_ wrapped in a record. The issue is with how
these lines *interact*. We started with our record type, then extracted a value.
At this point, the compiler has to hold onto its value, the `id` function, and
its type, `forall k a. Category k => k a a`. At this point, we can
observe something a bit weird: in this signature, the `a` type could correspond to our
original record type!

In fact, with this step, we've made a *more general* type signature, which is
not something that the compiler wants to do. After all, if we keep getting more
general, how will we ever solve the type equations? This situation, (where a
type signature is general enough to contain the structure containing it), is
called **impredicativity**.

The next question is: why did `foo` work when we specialised it to `a -> a`? The
answer to this is that the compiler actually can work certain situations out,
but ends up struggling when we introduce *type constraints*. Why? Simply because
the capability isn't in the compiler at the moment. While we _can_ talk about type
variables, the fact is that this particular transformation would require us to be
able to store constraints on the type variables, too. Not having an allowance for
impredicative types will make very little difference to us in practice, but does
mean that we might sometimes need to work around the issue.

So... why does this matter to _lenses_? 

## Conclusion (The Short Answer)

Well, let's take another look at that first failing example:

```purescript
newtype Wrapped = Wrapped String
derive instance newtypeWrapped :: Newtype Wrapped _

type Example = { lens :: Iso' Wrapped String }

bad :: Example -> Lens' Wrapped String
bad = _.lens
```

The optic types in this library are defined using various forms of the
`Profunctor` constraint. That means that our example here will fail to pass
type-checking, despite looking reasonable. To get around this, we just need to
use a different form of the optic that doesn't have a visible constraint:

```purescript
type Example = { lens :: AnIso' Wrapped String }

bad :: Example -> Lens' Wrapped String
bad = cloneLens <<< _.lens
```

Here, instead of using `Iso'`, we use `AnIso'`. There's a difference in the type
signatures:

```purescript
-- The main comparison
type Iso'   s a = Iso s s a a
type AnIso' s a = Optic (Exchange a a) s s a a

-- For context
type Optic :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Type
type Optic p s t a b = p a b -> p s t

type Iso s t a b = forall p. Profunctor p => Optic p s t a b

data Exchange a b s t = Exchange (s -> a) (b -> t)
```

What's the difference? Well, if you explore `Optic` and `Exchange`, you won't
find any constraints! All we're doing here is side-stepping the impredicativity
issue. Afterwards, we can use `cloneIso` to turn our `AnIso` back into the `Iso`
that we've always wanted.

In short, we used the optics prefixed with `A` or `An` to avoid impredicativity
issues, and then `clone` those optics to recover the original lens. `Exchange`
is a data type that characterises `Iso`, but there are similar types for all the
favourites:

| Profunctor Optic | Profunctor | `data` Optic | Inner Type | Recovery Function |
| -- | -- | -- | -- | -- |
| `Iso` | `Profunctor` | `AnIso` | `Exchange` | `cloneIso` |
| `Lens` | `Strong` | `ALens` | `Shop` | `cloneLens` |
| `IndexedLens` | `Strong` | `AnIndexedLens` | `Shop` | `cloneIndexedLens` |
| `Prism` | `Choice` | `APrism` | `Market` | `clonePrism` |
| `Grate` | `Closed` | `AGrate` | `Grating` | `cloneGrate` |

So, when you run into a strange type error that doesn't look like a problem,
consider impredicativity, and see whether one of the analogous optics and its
recovery function solve the issue!
