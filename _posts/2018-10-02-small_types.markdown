---
title: "Keep your types small..."
date: 2018-10-02
layout: post
categories: programming
---

# ... and your bugs smaller

In my post ["Type Safety Back and Forth"]({% post_url 2017-10-11-type_safety_back_and_forth %}), I discussed two different techniques for bringing type safety to programs that may fail.
On the one hand, you can push the responsibility forward.
This technique uses types like `Either` and `Maybe` to report a problem with the inputs to the function.
Here are two example type signatures:

```haskell
safeDivide
    :: Int
    -> Int
    -> Maybe Int

lookup
    :: Ord k
    => k
    -> Map k a
    -> Maybe a
```

If the second parameter to `safeDivide` is `0`, then we return `Nothing`.
Likewise, if the given `k` is not present in the `Map`, then we return `Nothing`.

On the other hand, you can push it back.
Here are those functions, but with the safety pushed back:

```haskell
safeDivide
    :: Int
    -> NonZero Int
    -> Int

lookupJustified
    :: Key ph k
    -> Map ph k a
    -> a
```

With `safeDivide`, we require the user pass in a `NonZero Int` -- a type that guarantees that the underlying value is not `0`.
With `lookupJustified`, the `ph` type guarantees that the `Key` is present in the `Map`, so we can pull the resulting value out without requiring a `Maybe`.
(Check out the [tutorial](https://hackage.haskell.org/package/justified-containers-0.3.0.0/docs/Data-Map-Justified-Tutorial.html) for `justified-containers`, it is pretty awesome)

# Expansion and Restriction

"Type Safety Back and Forth" uses the metaphor of "pushing" the responsibility in one of two directions: 

- forwards: the caller of the function is responsible for handling the possible error output
- backwards: the caller of the function is required to providing correct inputs

However, this metaphor is a bit squishy.
We can make it more precise by talking about the "cardinality" of a type -- how many values it can contain.
The type `Bool` can contain two values -- `True` and `False`, so we say it has a cardinality of 2.
The type `Word8` can express the numbers from 0 to 255, so we say it has a cardinality of 256.

The type `Maybe a` has a cardinality of `1 + a`.
We get a "free" value `Nothing :: Maybe a`.
For every value of type `a`, we can wrap it in `Just`.

The type `Either e a` has a cardinality of `e + a`.
We can wrap all the values of type `e` in `Left`, and then we can wrap all the values of type `a` in `Right`.

The first technique -- pushing forward -- is "expanding the result type."
When we wrap our results in `Maybe`, `Either`, and similar types, we're saying that we can't handle all possible inputs, and so we must have extra outputs to safely deal with this.

Let's consider the second technique.
Specifically, here's `NonZero` and `NonEmpty`, two common ways to implement it:

```haskell
newtype NonZero a 
    = UnsafeNonZero 
    { unNonZero :: a 
    }

nonZero :: (Num a, Eq a) => a -> Maybe (NonZero a)
nonZero 0 = Nothing
nonZero i = Just (UnsafeNonZero i)

data NonEmpty a = a :| [a]

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (x:xs) = x :| xs
```

What is the cardinality of these types?

`NonZero a` represents "the type of values `a` such that the value is not equal to `0`."
`NonEmpty a` represents "the type of lists of `a` that are not empty."
In both of these cases, we start with some larger type and remove some potential values.
So the type `NonZero a` has the cardinality `a - 1`, and the type `NonEmpty a` has the cardinality `[a] - 1`.

Interestingly enough, `[a]` has an infinite cardinality, so `[a] - 1` seems somewhat strange -- it is also infinite!
Math tells us that these are even the *same* infinity.
So it's not the mere cardinality that helps -- it is the specific value(s) that we have removed that makes this type safer for certain operations.

These are custom examples of [refinement types](https://ucsd-progsys.github.io/liquidhaskell-tutorial/).
Another closely related idea is [quotient types](https://www.hedonisticlearning.com/posts/quotient-types-for-programmers.html).
The basic idea here is to *restrict* the size of our inputs.
Slightly more formally,

- Forwards: expand the range
- Backwards: restrict the domain

# Constraints Liberate

Runar Bjarnason has a wonderful talk titled [Constraints Liberate, Liberties Constrain](https://www.youtube.com/watch?v=GqmsQeSzMdw).
The big idea of the talk, as I see it, is this:

> When we restrict what we can do, it's easier to understand what we can do.

I feel there is a deep connection between this idea and Rich Hickey's talk [Simple Made Easy](https://www.youtube.com/watch?v=34_L7t7fD_U).
In both cases, we are focusing on simplicity -- on cutting away the inessential and striving for more elegant ways to express our problems.

Pushing the safety forward -- expanding the range -- does not make things simpler.
It provides us with more power, more options, and more possibilities.
Pushing the safety backwards -- restricting the domain -- does make things simpler.
We can use this technique to take away the power to get it wrong, the options that aren't right, and the possibilities we don't want.

Indeed, if we manage to restrict our types sufficiently, there may be only one implementation possible!
The classic example is the `identity` function:

```haskell
identity :: a -> a
identity a = a
```

This is the only implementation of this function that satisfies the type signature (ignoring `undefined`, of course).
In fact, for any function with a sufficiently precise type signature, there is a way to automatically derive the function!
Joachim Breitner's [`justDoIt`](https://www.joachim-breitner.de/blog/735-The_magic_%E2%80%9CJust_do_it%E2%80%9D_type_class) is a fascinating utility that can solve these implementations for you.

With sufficiently fancy types, the computer can write even more code for you.
The programming language Idris can [write well-defined functions like `zipWith` and `tranpose` for length-indexed lists nearly automatically!](https://youtu.be/X36ye-1x_HQ?t=1140)

# Restrict the Range

I see this pattern and I am compelled to fill it in:

|        | Restrict | Expand |
|--------|----------|--------|
| Range  |          |   :(   |
| Domain |    :D    |        |

I've talked about restricting the range and expanding the domain.
Expanding the domain seems silly to do -- we accept more possible values than we know what to do with.
This is clearly not going to make it easier or simpler to implement our programs.
However, there are many functions in Haskell's standard library that have a domain that is too large.
Consider:

```haskell
take :: Int -> [a] -> [a]
```

`Int`, as a domain, is both too large and too small.
It allows us to provide negative numbers: what does it even mean to take `-3` elements from a list?
As `Int` is a finite type, and `[a]` is infinite, we are restricted to only using this function with sufficiently small `Int`s.
A closer fit would be `take :: Natural -> [a] -> [a]`.
`Natural` allows any non-negative whole number, and perfectly expresses the reasonable domain.
Expanding the domain isn't desirable, as we might expect.

`base` has functions with a *range* that is too large, as well.
Let's consider:

```haskell
length :: [a] -> Int
```

This has many of the same problems as `take` -- a list with too many elements will overflow the `Int`, and we won't get the right answer.
Additionally, we have a guarantee that we *forget* -- a `length` for any container must be positive!
We can more correctly express this type by *restricting* the output type:

```haskell
length :: [a] -> Natural
```

# A perfect fit

The more precisely our types describe our program, the fewer ways we have to go wrong.
Ideally, we can provide a correct output for every input, and we use a type that tightly describes the properties of possible outputs.
