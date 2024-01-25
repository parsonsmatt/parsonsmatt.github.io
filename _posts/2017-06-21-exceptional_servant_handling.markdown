---
title: "Exceptional Servant Handling"
date: 2017-06-21
layout: post
categories: programming
---

The [Haskell Servant](http://haskell-servant.readthedocs.io/en/stable/) library is a fantastic way to write web APIs.
When you're implementing the handlers, you either need to write them in the `Handler` monad, or define a `Nat`ural transformation (a way of converting) your choice of monad into the `Handler` monad.
The [`Handler` monad](http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#the-handler-monad) is a `newtype` around `ExceptT ServantErr IO a`, where [`ServantErr`](https://hackage.haskell.org/package/servant-server-0.11/docs/Servant-Server.html#t:ServantErr) is a way of providing errors like `404 -- Not Found` or `401 -- Not Authorized`, or other non-200 responses, like `302` redirection.

If you're familiar with `ExceptT`, this isn't new to you.
You can always use `throwError` in `ExceptT` to short-circuit the block and return the given error.
Servant handles the `ServantErr` intelligently, converting it into an appropriate response.
For non-`ServantErr` exceptions, Servant lets the serving backend (typically [`WAI`](https://hackage.haskell.org/package/wai)) handle it, usually by providing a `500` error.

# `ExceptT e IO` antipattern

Perhaps you've read Michael Snoyman's [Exceptions Best Practices In Haskell](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell) blog post.
Perhaps you're sold on the idea -- why bother with `ExceptT` over `IO` when `IO` already can throw runtime errors?
Furthermore, maybe you're concerned with performance -- the `>>=` implementation for `ExceptT` must do case analysis on the result to determine what to do next.
Carter Schonwald's [monad-ste](https://hackage.haskell.org/package/monad-ste) package provides a more efficient way of dealing with exceptions, as it uses GHC's runtime exception system.

There are various good reasons why you might want to strip `ExceptT` from your Servant handlers.
There are various good reasons why you wouldn't want to do that.
I'm in the first camp -- I don't want `ExceptT` over `IO`.

Maybe you don't even like monad transformers at all, and just want your handlers to be in plain ol' `IO`.

![stupid expanding brain meme]({{ site.url }}/brain-meme.jpg-large)

Well, it turns out, that doesn't take much code!

# Nat Simplification

The `servant-server` library allows you to use the function [`enter`](https://hackage.haskell.org/package/servant-server-0.11/docs/Servant-Server.html#g:5) to provide a conversion function from one monad to another.

Given an API type like:

```haskell
type API =
  "best-numbers" :> Get '[JSON] [Int]
```

we can write a handler like this:

```haskell
server :: IO [Int]
server = do
  now <- getCurrentTime
  let timeInSeconds = utctDayTime now
      wakeUpTime = 8 * 60 * 60
  when (timeInSeconds <= wakeUpTime) $
    throwIO err400 { 
      errBody = "request too early!" 
    }

  return [1,2,3]
```

This handler returns `[1, 2, 3]` if it's awake.
But if the current time is less than 8 AM UTCTime, then we throw a 400 error instead.
Since `ServantErr` is an instance of `Exception`, we're allowed to throw it in `IO` using `throwIO :: Exception e => e -> IO a`.
If you're using the `exceptions` package, you can use `throwM` as well.

To hook this up with the `serve` function, we need to use `enter` and provide a [`NT`](https://hackage.haskell.org/package/servant-server-0.11/docs/Servant-Server.html#t::-126--62-) natural transformation/conversion function.
The type signature in the documentation is super generic, but ultimately, we're looking for a function like:

```haskell
type NaturalTransformation source target
  = forall a. source a -> target a
```

Or, in English, "a function that converts a `source a` into a `target a` that is forbidden from inspecting the `a` values."
Concretely, for our specific use case, we want:

```haskell
convert :: IO a -> Handler a
```

# Hole Driven Development

Hole driven development to the rescue!
HDD is where you create a type hole and fill it in with your 'best guess' based on the surrounding context.
Typically you'll drop another type hole, which allows you to interactively develop with the compiler.

```haskell
convert :: IO a -> Handler a
convert action = _f
```

Well, `_f` gives us a type hole for `Handler a`, which isn't surprising.
How can we construct a [`Handler`](https://hackage.haskell.org/package/servant-server-0.11/docs/Servant-Server.html#t:Handler)?
The Haddocks point us to an exposed constructor, also `Handler`, which we can use.

```haskell
convert :: IO a -> Handler a
convert action = Handler _f
```

Now, `_f` is `ExceptT ServantErr IO a`.
How do we construct  an [`ExceptT`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html#t:ExceptT)?
The Haddocks show that we have a constructor, also called `ExceptT`, which expects an `m (Either e a)`.
So let's plug that in:

```haskell
convert :: IO a -> Handler a
convert action = Handler (ExceptT _f)
```

Now `_f` is `IO (Either ServantErr a)`.
This is where it gets tricky.
We know that we have an `IO a` on hand.
If we do a hoogle search for [`IO a -> IO (Either e a)`](http://hoogle.haskell.org/?hoogle=IO+a+-%3E+IO+%28Either+ServantErr+a%29&scope=set%3Astackage), then we get a bunch of funny results.
None of them are exactly right, but there are a lot of variants on `try`.
So let's hoogle for [`try`](http://hoogle.haskell.org/?hoogle=try&scope=set%3Astackage)!

That gives us this nice definition:

```haskell
try :: Exception e => IO a -> IO (Either e a)
```

so let's plug that in:

```haskell
convert :: IO a -> Handler a
convert action = Handler (ExceptT (try _f))
```

Now, `_f` has the type `IO a`.
And we have an `IO a` already -- it's the parameter we've been passed!

So we can simplify our convert:

```haskell
convert :: IO a -> Handler a
convert = Handler . ExceptT . try
```

**NOTE**: This `convert` function is a no-op. `try`'s `e` type is instantiated to `ServerError` here, and no `IO a` actions you pass will throw `ServerError`, meanwhile any `IOException` exception thrown by IO actions will not be caught by `try`. So `convert` is basically doing nothing.

wrap it in the `NT` natural transformation newtype:

```haskell
convert :: IO :~> Handler
convert = NT . Handler . ExceptT . try
```

and use it in `enter`:

```haskell
app :: Application
app = server (Proxy :: Proxy Api) (enter convert handler)
```

Voila! You're throwing exceptions in IO, and Servant is still doing nothing with them.
