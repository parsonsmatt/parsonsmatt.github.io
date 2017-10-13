---
title: "Type Safety Back and Forth"
date: 2017-10-11
layout: post
categories: programming
---

Types are a powerful construct for improving program safety.
Haskell has a few notable ways of handling potential failure, the most famous being the venerable `Maybe` type:

```haskell
data Maybe a
    = Just a
    | Nothing
```

We can use `Maybe` as the result of a function to indicate: 

> Hey, friend! This function might fail. You'll need to handle the `Nothing` case.

This allows us to write functions like a safe division function:

```haskell
safeDivide :: Int -> Int -> Maybe Int
safeDivide i 0 = Nothing
safeDivide i j = Just (i `div` j)
```

I like to think of this as pushing the responsibility for failure forward.
I'm telling the caller of the code that they can provide whatever `Int`s they want, but that some condition might cause them to fail.
And the caller of the code has to handle that failure later on.

This is the easiest technique to show and tell, because it's one-size-fits-all.
If your function can fail, just slap `Maybe` or `Either` on the result type and you've got safety.
I can write a 35 line blog post to show off the technique, and if I were feeling frisky, I could use it as an introduction to `Functor`, `Monad`, and all that jazz.

Instead, I'd like to share another technique.
Rather than push the responsibility for failure forward, let's explore pushing it back.
This technique is a little harder to show, because it depends on the individual cases you might use.

If pushing responsibility forward means accepting whatever parameters and having the caller of the code handle possibility of failure, then *pushing it back* is going to mean we accept stricter parameters that we can't fail with.
Let's consider `safeDivide`, but with a more lax type signature:

```haskell
safeDivide :: String -> String -> Maybe Int
safeDivide iStr jStr = do
    i <- readMay iStr
    j <- readMay jStr
    guard (j /= 0)
    pure (i `div` j)
```

This function takes two strings, and then tries to parse `Int`s out of them.
Then, if the `j` parameter isn't `0`, we return the result of division.
This function is *safe*, but we have a much larger space of calls to `safeDivide` that fail and return `Nothing`.
We've accepted more parameters, but we've pushed a lot of responsibility forward for handling possible failure.

Let's push the failure back.

```haskell
safeDivide :: Int -> NonZero Int -> Int
safeDivide i (NonZero j) = i `div` j
```

We've required that users provide us a `NonZero Int` rather than any old `Int`.
We've pushed back against the callers of our function:

> No! You must provide a `NonZero Int`. I refuse to work with just any `Int`, because then I might fail, and that's annoying.

So speaks our valiant little function, standing up for itself!

Let's implement `NonZero`.
We'll take advantage of Haskell's `PatternSynonyms` language extension to allow people to pattern match on a "constructor" without exposing a way to unsafely construct values.

```haskell
{-# LANGUAGE PatternSynonyms #-}

module NonZero 
  ( NonZero(getNonZero)
  , pattern NonZero
  , nonZero
  ) where

newtype NonZero a = UnsafeNonZero { unNonZero :: a }

pattern NonZero a = UnsafeNonZero a

nonZero :: (Num a, Eq a) => a -> Maybe (NonZero a)
nonZero 0 = Nothing
nonZero i = Just (UnsafeNonZero i)
```

This module allows us to push the responsibilty for type safety backwards onto callers.

As another example, consider `head`.
Here's the unsafe, convenient variety:

```haskell
head :: [a] -> a
head (x:xs) = x
head []     = error "oh no"
```

This code is making a promise that it can't keep.
Given the empty list, it will fail at runtime.

Let's push the responsibility for safety forward:

```haskell
headMay :: [a] -> Maybe a
headMay (x:xs) = Just x
headMay []     = Nothing
```

Now, we won't fail at runtime.
We've required the caller to handle a `Nothing` case.

Let's try pushing it back now:

```haskell
headOr :: a -> [a] -> a
headOr def (x:xs) = x
headOr def []     = def
```

Now, we're requiring that the *caller* of the function handle possible failure before they ever call this.
There's no way to get it wrong.
Alternatively, we can use a type for nonempty lists!

```haskell
newtype NonEmpty a = a :| [a]

safeHead :: NonEmpty a -> a
safeHead (x :| xs) = x
```

This one works just as well.
We're requiring that the calling code handle failure ahead of time.

A more complicated example of this technique is the [`justified-containers`](https://hackage.haskell.org/package/justified-containers-0.1.2.0/docs/Data-Map-Justified-Tutorial.html) library.
The library uses the type system to prove that a given key exists in the underlying `Map`.
From that point on, lookups using those keys are *total*: they are guaranteed to return a value, and they don't return a `Maybe`.

# The Ripple Effect

When some piece of code hands us responsibility, we have two choices:

1. Handle that responsibility.
2. Pass it to someone else!

In my experience, developers will tend to push responsibility in the same direction that the code they call does.
So if some function returns a `Maybe`, the developer is going to be inclined to also return a `Maybe` value.
If some function requires a `NonEmpty Int`, then the developer is going to be inclined to *also* require a `NonEmpty Int` be passed in.

This played out in my work codebase.
We have a type representing an `Order` with many `Item`s in it.
Originally, the type looked something like this:

```haskell
data Order = Order  { items :: [Item] }
```

The `Item`s contained nearly all of the interesting information in the order, so almost everything that we did with an `Order` would need to return a `Maybe` value to handle the empty list case.
This was a lot of work, and a lot of `Maybe` values!

The type is *too permissive*.
As it happens, an `Order` may not exist without at least one `Item`.
So we can make the type *more restrictive* and have more fun!

We redefined the type to be:

```haskell
data Order = Order { items :: NonEmpty Item }
```

All of the `Maybe`s relating to the empty list were purged, and all of the code was pure and free.
The failure case (an empty list of orders) was moved to two sites:

1. Decoding JSON
2. Decoding database rows

Decoding JSON happens at the API side of things, when various services `POST` updates to us.
Now, we can respond with a `400` error and tell API clients that they've provided invalid data!
This prevents our data from going bad.

Decoding database rows is even easier.
We use an `INNER JOIN` when retrieving `Order`s and `Item`s, which guarantees that each `Order` will have at least one `Item` in the result set.
Foreign keys ensure that each `Item`'s `Order` is actually present in the database.
This does leave the possibility that an `Order` might be orphaned in the database, but it's mostly safe.

When we push our type safety back, we're encouraged to continue pushing it back.
Eventually, we push it all the way back -- to the edges of our system!
This simplifies all of the code and logic inside of the system.
We're taking advantage of types to make our code simpler, safer, and easier to understand.

# Ask Only What You Need

In many senses, designing our code with type safety in mind is about being as strict as possible about your possible inputs.
Haskell makes this easier than many other languages, but there's nothing stopping you from writing a function that can take literally any binary value, do whatever effects you want, and return whatever binary value:

```haskell
foobar :: ByteString -> IO ByteString
```

A `ByteString` is a totally unrestricted data type.
It can contain any sequence of bytes.
Because it can express any value, we have very little guarantees on what it actually contains, and we are very limited in how we can safely handle this.

By restricting our past, we gain freedom in the future.
