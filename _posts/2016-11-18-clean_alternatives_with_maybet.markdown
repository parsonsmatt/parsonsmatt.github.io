---
title: "Clean Alternatives with MaybeT"
date: 2016-11-18
layout: post
categories: programming
---

Haskell's abstraction facilities are awesome.
`Functor`, `Applicative`, and `Monad` are all great, and `Maybe` is a pretty fantastic example of each.
Lifting functions over optional values, combining optional values, and sequencing the possibility of `Nothing`ness are pretty powerful tools for cleaning up code.
The first time I refactored some `Maybe` infested code like:

```haskell
someFunc :: Int -> Maybe String
someFunc i =
    case foo i of
        Nothing -> Nothing
        Just a ->
            case bar a of
                Nothing -> Nothing
                Just b -> Just (show b)
```

into the elegant:

```haskell
someFunc i = do
    a <- foo i
    b <- bar a
    pure (show b)
```

I knew I was totally hooked.

The `Monad` instance for `Maybe` covers a common case: given some sequence of functions which may fail, we want to try them all and if any of them fail then we'll short circuit it all.
However, that's not the only case.
Very often, you'll want to take the first thing that succeeds, rather than failing unless everything works. Something like this:

```haskell
someOtherFunc :: Int -> Maybe String
someOtherFunc i = do
    case foo i of
        Just a -> Just a
        Nothing ->
            case bar i of
                Just b -> Just b
                Nothing ->
                    wat i
```

One of Haskell's lesser known type classes is `Alternative`, which is precisely the abstraction we want here!

# Alternative

```haskell
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
```

The `Alternative` class gives us `empty`, which is an "empty" value, and `<|>`, which allows us to define a way to choose between two values.
The [documentation](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html#t:Alternative) tells us that `empty` should be an identity for `<|>`, and that `<|>` is a binary associative operator (huh, sounds like a monoid, right?)

Maybe has a nice `Alternative` instance that looks like this:

```haskell
instance Alternative Maybe where
    empty = Nothing
    Just a  <|> _ = Just a
    Nothing <|> b = b
```

Does this make sense? Well, if we have some `Just 10`, and we choose between `Nothing <|> Just 10`, then we'll pick `Just 10`.
Likewise, if we choose between `Just 10 <|> Nothing`, we'll take `Just 10`.
It's associative, so we don't need parentheses.
`a <|> b <|> c <|> d` will choose the first value that isn't `empty`.

Okay, so how can we rewrite `someOtherFunc` like this?

```haskell
someOtherFunc :: Int -> Maybe String
someOtherFunc i = foo i <|> bar i <|> wat i
```

Now that looks pretty nice! Definitely a lot cleaner than the previous one.

# Transformers In Disguise

Raise your hand if you've written some Haskell code like this:

```haskell
getFromCache     :: String -> IO (Maybe Record)
getFromDatabase  :: String -> IO (Maybe Record)
getFromRemoteAPI :: String -> IO (Maybe Record)

retrieveRecord :: String -> IO (Maybe Record)
retrieveRecord name = do
    mrec <- getFromCache name
    case mrec of
        Just rec -> pure (Just rec)
        Nothing -> do
            mrec' <- getFromDatabase name
            case mrec' of
                Just rec -> pure (Just rec)
                Nothing ->
                    getFromRemoteAPI name
```

GROSS! That's just as bad as before.
Wouldn't it be great if we could get that nice `Maybe` `Alternative` action going here?

Well, we can!
The entire magic of a monad transformer is that we can *enhance* a base monad with features of another monad.
Let's cover the implementation of `MaybeT` and see how to use it to wrap our actions and get that choice.

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

I'm going to elide the `Functor` and `Applicative` definitions -- let's get right into `Monad`:

```haskell
instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    MaybeT ma >>= f = ???
```

`???` has the type `MaybeT m b`, `ma :: m (Maybe a)`, and `f :: a -> MaybeT m b`.
We need to get the `a` out of that `ma` value, but it's a `Monad`, so we can only `bind` out of it.
So we'll have to start with the `MaybeT` constructor.

```haskell
instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    MaybeT ma >>= f = MaybeT ???
```

The `???` value has the type `m (Maybe b)` now, which means that it's in the same monad.
This means we can use `do` and bind out of that original `ma` value!

```haskell
instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    MaybeT ma >>= f = MaybeT $ do
        maybeA <- ma
        ???
```

We've got a `maybeA :: Maybe a` value now, so we're not out of the weeds yet.
We'll case match on the value.
If it's `Nothing`, we'll `return Nothing` since we can't do anything else.
Otherwise, we can continue!

```haskell
instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    MaybeT ma >>= f = MaybeT $ do
        maybeA <- ma
        case maybeA of
            Nothing -> return Nothing
            Just a -> ???
```

Now that we've finally got that `a`, we need to apply it to `f`.

```haskell
instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    MaybeT ma >>= f = MaybeT $ do
        maybeA <- ma
        case maybeA of
            Nothing -> return Nothing
            Just a -> f a
```

However, this isn't quite right, because `f a :: MaybeT m b`, and we need `m (Maybe b)`! We'll unwrap with `runMaybeT` and it'll work.

```haskell
instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    MaybeT ma >>= f = MaybeT $ do
        maybeA <- ma
        case maybeA of
            Nothing -> return Nothing
            Just a -> runMaybeT (f a)
```

Cool! Now, what's the alternative instance look like?

```haskell
instance Monad m => Alternative (MaybeT m a) where
    empty = MaybeT (return Nothing)
    MaybeT first <|> MaybeT second = ???
```

Well, we'll want to check the first value, and if it's `Nothing`, then we'll check the second value.

```haskell
instance Monad m => Alternative (MaybeT m a) where
    empty = MaybeT (return Nothing)
    MaybeT first <|> MaybeT second = MaybeT $ do
        maybeA <- first
        case maybeA of
            Just a -> return (Just a)
            Nothing -> ???
```

Well, now we've taken care of the `first` action.
If it was `Nothing`, then we'll need to bind out of the `second` action.

```haskell
instance Monad m => Alternative (MaybeT m a) where
    empty = MaybeT (return Nothing)
    MaybeT first <|> MaybeT second = MaybeT $ do
        maybeA <- first
        case maybeA of
            Just a -> return (Just a)
            Nothing -> do
                maybeA' <- second
                case maybeA' of
                    Just a -> return (Just a)
                    Nothing -> return Nothing
```

There's some redundancy here that we can clean up:

```haskell
instance Monad m => Alternative (MaybeT m a) where
    empty = MaybeT (return Nothing)
    MaybeT first <|> MaybeT second = MaybeT $ do
        maybeA <- first
        case maybeA of
            Just a -> return (Just a)
            Nothing -> second
```

Cool!

# Using the Alternative

Now that we've got our `Alternative`, we can use it with our previous functions:


```haskell
getFromCache     :: String -> IO (Maybe Record)
getFromDatabase  :: String -> IO (Maybe Record)
getFromRemoteAPI :: String -> IO (Maybe Record)

retrieveRecord :: String -> IO (Maybe Record)
retrieveRecord name = runMaybeT 
     $  MaybeT (getFromCache name)
    <|> MaybeT (getFromDatabase name)
    <|> MaybeT (getFromRemoteAPI name)
```

This is a lot cleaner!
I hope this has convinced you to check out the `Alternative` class and consider using it in your code.
