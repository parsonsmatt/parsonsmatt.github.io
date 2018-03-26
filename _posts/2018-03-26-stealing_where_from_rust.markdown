---
title: "Stealing Where from Rust"
date: 2018-03-26
layout: post
categories: programming
---

I normally write about Haskell, but I'm also super excited about Rust!
I'm pretty new to it, though, so this post might have some incorrectness.
One convenient feature that Rust has is a type-level `where`.
It's used to provide trait bounds (equivalent to Haskell's type class constraints) to type variables.

Here's a type signature from `nom`, a parser combinator library ([github link](https://github.com/Geal/nom/blob/b478db511696ddbc7ff6ee6a012dd50bc3a6789e/src/character.rs#L141-L143)):

```rust
pub fn anychar<T>(input: T) -> IResult<T, char>
where
  T: InputIter + InputLength + Slice<RangeFrom<usize>> + AtEof,
  <T as InputIter>::Item: AsChar,
```

We're declaring a *pub*lic *f*u*n*ction named `anychar` that is parameterized by some type `T`.
It accepts a single argument, named `input`, that has the type `T`.
It returns a value of type `IResult<T, char>`.

The `where` bit defines the trait bounds on the `T` type -- we say that `T` must satisfy the `InputIter`, `InputLength`, `Slice`, etc. traits.
The `::` refers to an associated type, which is essentially the same thing as an associated type on a type class in Haskell.
So `<T as InputIter>::Item` refers to the `Item` type for the implementation of `T`'s `InputIter` trait.

Let's write this in Haskell:

```haskell
anychar 
    :: forall t. 
    ( InputIter t, InputLength t
    , Slice (RangeFrom Int) t, AtEof t
    , AsChar (InputIterItem t)
    ) => t -> Result t Char
```

Haskell has *implicit quantification* by default.
That is, you don't have to introduce type variables -- you can just use them where you want.
The `forall` keyword introduces type variables to a type.
So the syntax `forall t.` is analogous to the `<T>` syntax in Rust.

Haskell's type classes take type variables as arguments and turn them into `Constraint`s.
So, instead of Rust's `t : InputIter` ("`t` satisfies the trait bound `InputIter`"), we say `InputIter t` ("given an instance of `InputIter` for `t`").

Can we get this into Haskell, as well?
Yes, with some type families!

```haskell
type family Where a cs :: Constraint where
    Where _ '[]       = ()
    Where a (c ': cs) = (c a, Where a cs)
```

Here, we define a type family `Where`, which takes two parameters: a type `a` of kind `k`, and a type `cs` of kind `[k -> Constraint]`, and it turns them into a single `Constraint`.
For the empty list, we have no constraints to add -- therefore, we use the empty constraint, `()`.
If we have a constraint, then we add that constraint `c a` and do constraint union with the result of `Where a cs`.

Here's how it looks:

```haskell
anychar :: forall t.
     ( Where t 
        [ InputIter, InputLength
        , Slice (FromRange Int), AtEof
        ]
     , AsChar (InputIterItem t)
     ) => f a -> f a
```

Nice.

It might be good to remember the story "If you give a mouse a cookie..."

# If you give a Haskeller a syntax, 

## they'll just want more!

If we can have `Where`, that makes me want `Let`.
It would be really nice to be able to provide shorthands for commonly repeated types in a function.
Consider this signature:

```haskell
doThings
    :: MaybeT (ExceptT MyError IO) Int
    -> MaybeT (ExceptT MyError IO) Char
    -> MaybeT (ExceptT MyError IO) (Int, Char)
doThings mi mc = do
    i <- mi
    c <- mi
    pure (i, c)
```

Look at all of that repetition.
We can factor it out into a top level definition:

```haskell
type ThingDoing = MaybeT (ExceptT MyError IO)

doThings
    :: ThingDoing Int
    -> ThingDoing Char
    -> ThingDoing (Int, Char)
```

But that can clutter the namespace.
We want something local, for the same reason we want `let` and `where` in the value language.
What do we have to do to make the following code work?

```haskell
doThings
    :: Let m (MaybeT (ExceptT MyError IO))
    => m Int
    -> m Char
    -> m (Int, Char)
```

Well, it's easier than you might think:

```haskell
type Let = (~)
```

That squiggle `(~)` is a type equality constraint.
We're saying that `m` and `MaybeT (ExceptT MyError IO)` must be equal.
So that lets us use `m` where we might use the longer, explicit type.

This trick is great for reducing duplication in type signatures.
Thanks, Rust, for inspiring me to want this.
