---
title: "MonadBaseControl in Five Minutes"
date: 2017-11-21
layout: post
categories: programming
---

This post is intended to be a short guide on using `MonadBaseControl` effectively in Haskell code without understanding it.

# Tiny synopsis

The big idea behind `MonadIO m` is that you can perform a transformation `IO a -> m a`.
The big idea behind `MonadBaseControl` is that you can perform a transformation `m a -> IO a`.

Most monads have additional context than just `IO`, so to go from your custom monad to `IO` requires providing additional context.
Additionally, many monads alter the return type slighty.
`ExceptT e IO a` turns into `IO (Either e a)`, and `StateT s IO a` turns into `IO (a, s)`.
The `StM m a` type family is used to associate the types.

There is one function you need to know: `control`.

# Lifting Callbacks

The primary reason to use `MonadBaseControl` is to lift IO callbacks.
Here are some examples:

```haskell
withFile 
    :: FilePath 
    -> IOMode 
    -> (Handle -> IO a) -- callback to lift
    -> IO a

withFileLifted
    :: (MonadBaseControl IO m, StM m a ~ a)
    => FilePath
    -> IOMode
    -> (Handle -> m a)
    -> m a
withFileLifted path mode action =
    control $ \runInIO ->
        withFile path mode (\handle -> 
            runInIO (action handle))
```

The `StM m a ~ a` line asserts that the `m` monad does not alter the return state.
This means no `ExceptT`, `StateT`, etc.
Those have unpredictable effects in the presence of multithreading, so it's best to avoid them.

Here is another example:

```haskell
forkIO :: IO a -> IO ThreadId

forkIOLifted 
    :: (MonadBaseControl IO m)
    => m a
    -> m (StM m ThreadId)
forkIOLifted action =
    control $ \runInIO ->
        forkIO (runInIO action)
```

The general pattern is to write:

```haskell
control $ \runInIO -> do   -- [1]
    putStrLn "go"
    res <- runInIO foobar  -- [2]
    putStrLn "end"
    return res             -- [3]
```

1. `control` takes a callback that operates in `IO`.
2. It provides a function for dropping the extra stuff and running an lifted action as an IO action.
3. When you return something, it needs the `StM m a` type to know how to augment the value.

# With Exceptions

There have been many questions on converting `MonadBaseControl` and `MonadCatch` sorts of functions.
If you have a function `foo :: MonadCatch m => m a`, then you can *specialize* the type to any `MonadCatch` instance.
For `(MonadCatch m, MonadIO m)`, you can specialize to `m ~ IO`.
Finally, you can use `control` to generalize it.

Given:

```haskell
import Control.Exception.Safe

catch 
    :: (MonadCatch m, Exception e)
    => m a
    -> (e -> m a)
    -> m a

catchLifted
    :: ( MonadBaseControl IO m
       , Exception e
       , StM m a ~ a
       )
    => m a
    -> (e -> m a)
    -> m a
catchLifted action handler =
    control $ \runInIO ->
        (runInIO action)
            `catch`
                (\e -> runInIO (handler e))
```

This works because we can specialize `MonadCatch m => m a` into `IO a`.
