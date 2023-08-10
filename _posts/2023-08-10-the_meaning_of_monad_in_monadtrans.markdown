---
title: "The Meaning of Monad in MonadTrans"
date: 2023-08-10
layout: post
categories: programming
---

At work, someone noticed that they got a compiler warning for a derived instance of `MonadTrans`.

```haskell
newtype FooT m a = FooT { unFooT :: StateT Int m a }
    deriving newtype
        (Functor, Applicative, Monad, MonadTrans)
```

GHC complained about a redundant `Monad` constraint.
After passing `-ddump-deriv`, I saw that GHC was pasting in basically this instance:

```haskell
instance MonadTrans FooT where
    lift :: Monad m => m a -> t m a
    lift = coerce (lift :: m a -> StateT Int m a)
```

The problem was that the `Monad m` constraint there is *redundant* - we're not actually using it.
However, it's a mirror for the definition of the class method.

In `transformers < 0.6`, the definition of `MonadTrans` class looked like this:

```haskell
class MonadTrans t where
    lift :: Monad m => m a -> t m a
```

In `transformers-0.6`, a quantified superclass constraint was added to `MonadTrans`:

```haskell
class (forall m. Monad m => Monad (t m)) => MonadTrans t where
    lift :: Monad m => m a -> t m a
```

I'm having a bit of semantic satiation with the word `Monad`, which isn't an unfamiliar phenomenon for a Haskell developer.
However, while explaining this behavior, I found it to be a very subtle distinction in what these constraints fundamentally *mean*.

# What is a Constraint?

A `Constraint` is a thing in Haskell that GHC needs to solve in order to make your code work.
Solving a constraint is similar to proving a proposition in constructive logic - GHC needs to find *evidence* that the claim holds, in the form of a type class instance.

When we write:

```haskell
foo :: Num a => a -> a
foo x = x * 2
```

We're saying: 

> I have a polymorphic function `foo` which can operate on types, *if* those types are instances of the class `Num`.

**If** is the big thing here - it's a way of making a conditional expression.
For a totally polymorphic function, like `id :: a -> a`, there are no *conditions*.
You can call it with any type you want.
But a *conditional* polymorphic function expresses some requirements, or *constraints*, upon the input.

If you ask for constraints you don't need, then you can get a warning by enabling `-Wredundant-constraints`.

```haskell
woops :: (Bounded a, Num a) => a -> a
woops x = x + 5
```

GHC will happily warn us that we don't actually *use* the `Bounded a` constraint, and it's redundant.
We should delete it.
Indeed, there are many `Num` that aren't `Bounded`, and by requiring `Bounded`, we are reducing the potential types we could call this function on for no reason.

# Constraints Liberate

(from [Constraints Liberate, Liberties Constrain](https://www.youtube.com/watch?v=GqmsQeSzMdw))

A constraint is a *perspective* on what is happening - it is the perspective of the *caller* of a function.
It's almost like I see a function type:

```haskell
someCoolFunction :: _ => a -> b
```

And think - "Ah hah! I can call this at any type `a` and `b` that I want!"
Only to find that there's a bunch of constraints on `a` and `b`, and now I am *constrained* in the types I can call this function at.

However, a constraint feels very different from the *implementer* of a function.
Let's look at the classic identity function:

```haskell
id :: a -> a
id a = a
```

As an implementer, I have quite a few constraints!
Indeed, I can't really *do* anything here.
I can write equivalent functions, or much slower versions of this function, or I can escape hatch with unsafe behavior - but my options are really pretty limited.

```haskell
id' :: a -> a
id' a = repeat a !! 1000

id'' :: a -> a
id'' a = let y = a in y

id''' :: a -> a
id''' a = iterate id' a !! 1000
```

However, a `Constraint` means that I now have some extra power.

```haskell
foo :: Num a => a -> a
```

With this signature, I now have access to the `Num` type class methods, as well as any other function that is polymorphic over `Num`.
The constraint is a *liberty* - I have gained the power to *do stuff* with the input.

# The Two Monads

Back to `MonadTrans` -

```haskell
class
    (forall m. Monad m => Monad (t m))
  =>
    MonadTrans t
  where
    lift :: Monad m => m a -> t m a
```

## Method Constraint

Let's talk about that `lift` constraint.

```haskell
    lift :: Monad m => m a -> t m a
```

This constraint means that the *input* to `lift` must prove that it is a `Monad`.
This means that, as implementers of `lift`, we can use the methods on `Monad` in order to make `lift` work out.
We often don't *need* it - consider these instances.

```haskell
newtype IdentityT m a = IdentityT (m a)

instance MonadTrans IdentityT where
    lift action = IdentityT action
```

`IdentityT` can use the constructor directly, and does not require any methods at all to work with `action`.

```haskell
newtype ReaderT r m a = ReaderT (r -> m a)

instance MonadTrans (ReaderT r) where
    lift action = ReaderT $ \_ -> action
```

`ReaderT` uses the constructor and throws away the `r`.

```haskell
newtype ExceptT e m a = ExceptT (m (Either e a))

instance MonadTrans (ExceptT e) where
    lift action = ExceptT $ fmap Right action
```

Ah, now we're using the `Functor` method `fmap` in order to make the inner action fit.
We're given an `action :: m a`, and we need an `m (Either e a)`. And we've got `Right :: a -> Either e a` and `fmap` to make it work.
We are allowed to call `fmap` here because `Monad` implies `Applicative` implies `Functor`, and we've been *given* the `Monad m` constraint as part of the method.

## Superclass Constraint

Let's talk about the quantified superclass constraint (wow, what a fancy phrase).

```haskell
    (forall m. Monad m => Monad (t m))
```

This superclass constraint means that the type `t m a` is a `Monad` *if* `m` is a `Monad`, and this is true *for all `m`*, not just  a particular one.
Prior to this, if you wanted to write a `do` block thta was arbitrary in a monad transformer, you'd have to write:

```haskell
ohno :: (Monad (t m), Monad m, MonadTrans t) => m a -> t m a
ohno action = do
    lift action
    lift action
```

What's more annoying is that, if you had a few different underlying type parameters, you'd need to request the `Monad (t m)` for each one - `Monad (t m), Monad (t n), Monad (t f)`.
Boring and redundant.
Obviously, if `m` is a `Monad`, and `t` is a `MonadTrans`former, then `t m` *must also* be a `Monad` - otherwise it's not a valid `MonadTransformer`!

So this `Constraint` is slightly different.
The first perspective on `Constraint` is the user of a function:

> I am *constrained* in the types I can use a function with

The second perspective on `Constraint` is the implementer of a function:

> By *constraining* my inputs, I gain knowledge and power over them

But this superclass constraint is a bit different.
It doesn't seem to be about requiring anything from our users.
It also doesn't seem to be about allowing more power for implementers.

Instead, it's a form of *evidence propagation*.
We're saying:

> GHC, if you know that `m` is a `Monad`, then you may also infer that `t m` is a `Monad`.

Type classes form a compositional tool for logic programming.
Constraints like these are a conditional proposition that allow GHC to see more options for solving and satisfying problems.

# The Koan of `Constraint`

Four blind monks touch a `Constraint`, trying to identify what it is.

The first exclaims "This is a tool for limiting people."
The second laughs and says, "No, this is a tool for empowering people!"
The third shakes his head solemnly and retorts, "No, this is a tool for clarifying wisdom."

The fourth says "I cannot satisfy this."
