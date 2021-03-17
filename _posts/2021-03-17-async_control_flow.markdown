---
title: "Async Control Flow"
date: 2021-03-17
layout: post
categories: programming
---

This post is an investigation of [`persistent` issue #1199](https://github.com/yesodweb/persistent/issues/1199) where an asynchronous exception caused a database connnection to be improperly returned to the pool.
The linked issue contains some debugging notes, along with the [PR that fixes the problem](https://github.com/yesodweb/persistent/pull/1207).
While I was able to identify the problem and provide a fix, I don't really *understand* what happened - it's a complex bit of work.
So I'm going to write this up as an exploration into the exact code paths that are happening.

# `Data.Pool`

[`resource-pool`](https://hackage.haskell.org/package/resource-pool) is a how `persistent` manages concurrent pooling and sharing of database connections.
When you create a `Pool`, you specify how to create resources, destroy them, and then some information around resource management: how long to keep an unused resource open, how many sub-pools to maintain, and how many resources per sub-pool (aka stripe).
`persistent` calls `createPool` [here](https://github.com/yesodweb/persistent/blob/fdd7b7b42d2aaf8528cbe5e968002fef42e473fd/persistent/Database/Persist/Sql/Run.hs#L220-L238).
The database client libraries provide a `LogFunc -> IO SqlBackend` that is used to create new database connections, and the `close'` delegates to the `connClose` field on the `SqlBackend` record.

While `resource-pool` isn't seeing much maintenance activity, it's relatively well tested and reliable.
Once you've got a `Pool a` from `createPool`, the recommended way to use it is [`withResource`](https://www.stackage.org/haddock/lts-17.6/resource-pool-0.2.3.2/Data-Pool.html#v:withResource):

```haskell
withResource 
  :: (MonadBaseControl IO m)
  => Pool a 
  -> (a -> m b) 
  -> m b
withResource pool act = control $ \runInIO -> mask $ \restore -> do
  (resource, local) <- takeResource pool
  ret <- restore (runInIO (act resource)) `onException`
            destroyResource pool local resource
  putResource local resource
  return ret
```

# `Data.Acquire`

Now, in `persistent-2.10.5`, a new API based on the `resourcet` package's `Data.Acquire` [was introduced](https://github.com/yesodweb/persistent/pull/984), and this API became the underlying implementation for the `runSqlPool` family of functions.
The underlying functionality is in the new function [`unsafeAcquireSqlConnFromPool`](https://github.com/yesodweb/persistent/pull/984/files#diff-f9d7f232cd00cb88188b7fcc68110e3f4cb378fcad9df652360de44d13cd86e3R33-R45), which was later factored out into [`resourcet-pool`](https://hackage.haskell.org/package/resourcet-pool).
This change was introduced because `resource-pool` operates in `MonadBaseControl`, which is incompatible with many other monad transformers - specifically, `ConduitT`.
`Acquire` is based on `MonadUnliftIO`, which is compatible.

In hindsight, we could have just changed the code to use `MonadUnliftIO` - it's relatively straightforward to do.
A term with a single constrant like `MonadBaseControl IO m => m a` can be specialized to `IO a`, and we can then run that using `withRunInIO` from `unliftio`.

```haskell
toUnliftIO 
    :: MonadUnliftIO n
    => (forall m. MonadBaseControl IO m => m a)
    -> n a
toUnliftIO mbc = 
    withRunInIO $ \runInIO -> do
        mbc

toPlainIO 
    :: (forall m. MonadBaseControl IO m => m a)
    -> IO a
toPlainIO mbc = mbc

toMonadIO 
    :: MonadIO n
    => (forall m. MonadBaseControl IO m => m a)
    -> n a
toMonadIO mbc = liftIO (toPlainIO mbc)
```

# `Acquire` vs `Pool`

I didn't realize this at the time, but `Data.Acquire` is inherently a weaker tool than `Data.Pool`.
`Data.Acquire` provides a means of creating a new resource, and also freeing it automatically when a scope is exited.
`Data.Pool` keeps track of resources, resource counts, and occasionally destroys them if they're unsued.

So let's look at our conversion function:

```haskell
unsafeAcquireSqlConnFromPool = do
    pool <- MonadReader.ask

    let freeConn :: (backend, LocalPool backend) -> ReleaseType -> IO ()
        freeConn (res, localPool) relType = case relType of
            ReleaseException -> P.destroyResource pool localPool res
            _ -> P.putResource localPool res

    return $ fst <$> mkAcquireType (P.takeResource pool) freeConn
```

`mkAcquireType` is analogous to `createPool` - it creates a handle `Acquire a` that can be used with a function named [`with`](https://www.stackage.org/haddock/lts-17.6/resourcet-1.2.4.2/Data-Acquire.html#v:with):

```haskell
with :: MonadUnliftIO m
     => Acquire a
     -> (a -> m b)
     -> m b
with (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    Allocated x free <- f restore
    res <- restore (run (g x)) `E.onException` free ReleaseException
    free ReleaseNormal
    return res
```

`with` is aliased to `withAcquire`, which I'll use from here on out to disambiguate.

You may notice that `withAcquire` and `withResource` are implemented nearly identically.
`withResource` uses `MonadBaseControl` and `withAcquire` uses `MonadUnliftIO`, and that's the whole of the difference.
They have the same async exception handling with `mask` and use the same `onException` functions.
All the exception handling stuff is from `Control.Exception`, so we're not using `UnliftIO.Exception` or `Control.Monad.Catch` or `Control.Exception.Safe` here.

These are *really similar*.
When we look at how the `unsafeSqlConnFromPool` works, it should provide identical behavior.
For `free`, we case on `ReleaseType` and do `destroyResource` on exception and `putResource` on any other exit.

We're not handling `ReleaseEarly` specially - this constructor is only used when we use `ResourceT`'s `release` function on a value.
Using `withAcquire`, we'll only ever pass `ReleaseNormal` and `ReleaseException`.
So this is locally safe.
Weirdly enough, `resourcet` doesn't really depend on the `Acquire` type at all, at least not directly - the `ReleaseMap` type contains a function `ReleaseType -> IO ()` for freeing resources, but doesn't mention anything else about it.

Anyway, let's get back on track.
Since `withAcquire` and `withResource` are nearly identical, it may be our translating code that is the problem.
We can use algebraic substitution to check this out.
Let's look at [`mkAcquireType`](https://www.stackage.org/haddock/lts-17.6/resourcet-1.2.4.2/src/Data.Acquire.Internal.html#mkAcquireType):

```haskell
mkAcquireType
    :: IO a -- ^ acquire the resource
    -> (a -> ReleaseType -> IO ()) -- ^ free the resource
    -> Acquire a
mkAcquireType create free = Acquire $ \_ -> do
    x <- create
    return $! Allocated x (free x)
```

The ignored parameter in the lambda there is a function that looks like `restore` - and we're ignoring it.
So, this action gets run when we *unpack* the `Acquire` in `withAcquire`.
Let's plug in our `create` and `free` parameters:

```haskell
mkAcquireType
    :: IO a -- ^ acquire the resource
    -> (a -> ReleaseType -> IO ()) -- ^ free the resource
    -> Acquire a
mkAcquireType (create = P.takeResource pool) (free = freeConn) = Acquire $ \_ -> do
    x <- (P.takeResource pool)
    return $! Allocated x (freeConn x)
  where
    freeConn (res, localPool) relType = case relType of
        ReleaseException -> P.destroyResource pool localPool res
        _ -> P.putResource localPool res
```

The `pool` variable is captured in the closure.

Now we can look at `withAcquire`, and plug in our behavior:

```haskell
withAcquire (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    -- `f` ignores the `restore` argument: possible bug?
    Allocated x free <- f restore
    -- so `x` here comes from `P.takeResource pool`
    -- free = freeConn
    ret <- restore (run (g x)) 
        `E.onException` free ReleaseException
    free ReleaseNormal
    return ret
```

Let's plug in the specific case for `free`:

```haskell
withAcquire (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    -- `f` ignores the `restore` argument: possible bug?
    Allocated x free <- f restore
    -- so `x` here comes from `P.takeResource pool`
    -- free = freeConn
    ret <- restore (run (g x)) 
        `E.onException` P.destroyResource pool localPool res
    P.putResource localPool res
    return ret
```

Closer, closer... Let's unpack the `Allocated` stuff:

```haskell
withAcquire (Acquire _) g = withRunInIO $ \run -> E.mask $ \restore -> do
    -- `f` ignores the `restore` argument: possible bug?
    Allocated x free <- f restore
    -- so `x` here comes from `P.takeResource pool`
    -- free = freeConn
    ret <- restore (run (g x)) 
        `E.onException` P.destroyResource pool localPool res
    P.putResource localPool res
    return ret
  where
    f _ = do
        x <- (P.takeResource pool)
        return $! Allocated x (freeConn x)

-- OK, let's splice in the definition of `f`:

withAcquire (Acquire _) g = withRunInIO $ \run -> E.mask $ \restore -> do
    -- `f` ignores the `restore` argument: possible bug?
    Allocated x free <- do
        x <- P.takeResource pool
        return $! Allocated x (freeConn x)
    -- so `x` here comes from `P.takeResource pool`
    -- free = freeConn
    ret <- restore (run (g x)) 
        `E.onException` P.destroyResource pool localPool res
    P.putResource localPool res
    return ret

-- Now let's remove the `Allocated` constructor:

withAcquire (Acquire _) g = withRunInIO $ \run -> E.mask $ \restore -> do
    x@(res, localPool) <- P.takeResource pool
    ret <- restore (run (g x)) 
        `E.onException` P.destroyResource pool localPool res
    P.putResource localPool res
    return ret
```

With this, we're now nearly identical with `withResource` (copied again):

```haskell
withResource 
  :: (MonadBaseControl IO m)
  => Pool a 
  -> (a -> m b) 
  -> m b
withResource pool act = 
    control $ \runInIO -> mask $ \restore -> do
      (resource, local) <- takeResource pool
      ret <- restore (runInIO (act resource)) `onException`
                destroyResource pool local resource
      putResource local resource
      return ret
```

The *only* difference here is that `Acquire` also passes the `LocalPool` to the given action.
In the `persistent` code, we use `fmap fst` so that it only passes the resource to the callback.

So, I'm not sure this function is at fault.
Let's see how we call this function.

# What's that `>>=` doing there??

[`acquireSqlConnFromPool`](https://github.com/yesodweb/persistent/pull/984/files#diff-f9d7f232cd00cb88188b7fcc68110e3f4cb378fcad9df652360de44d13cd86e3R62-R67) is what's actually called by [`runSqlPool`](https://github.com/yesodweb/persistent/pull/984/files#diff-f9d7f232cd00cb88188b7fcc68110e3f4cb378fcad9df652360de44d13cd86e3R89) in this version of the code.

```haskell
acquireSqlConnFromPool
    :: (MonadReader (Pool backend) m, BackendCompatible SqlBackend backend)
    => m (Acquire backend)
acquireSqlConnFromPool = do
    connFromPool <- unsafeAcquireSqlConnFromPool
    return $ connFromPool >>= acquireSqlConn
```

That `>>=` is weird. What's going on here?
We have `return :: a -> m a`, and then `f >>= g`.
`f :: Acquire backend` - so then `g` must have the type `g :: backend -> Acquire backend`, meaning that we're using the `>>=` of `Acquire a -> (a -> Acquire b) -> Acquire b`.

`acquireSqlConn` cashes out to [`rawAcquireSqlConn`](https://github.com/yesodweb/persistent/pull/984/files#diff-f9d7f232cd00cb88188b7fcc68110e3f4cb378fcad9df652360de44d13cd86e3R122-R142):

```haskell
rawAcquireSqlConn
    :: forall backend m
     . (MonadReader backend m, BackendCompatible SqlBackend backend)
    => Maybe IsolationLevel -> m (Acquire backend)
rawAcquireSqlConn isolation = do
    conn <- MonadReader.ask
    let rawConn :: SqlBackend
        rawConn = projectBackend conn

        getter :: T.Text -> IO Statement
        getter = getStmtConn rawConn

        beginTransaction :: IO backend
        beginTransaction = conn <$ connBegin rawConn getter isolation

        finishTransaction :: backend -> ReleaseType -> IO ()
        finishTransaction _ relType = case relType of
            ReleaseException -> connRollback rawConn getter
            _ -> connCommit rawConn getter

    return $ mkAcquireType beginTransaction finishTransaction
```

So, in the investigation, the exception (`libpq: failed (another command is already in progress)`) would happen (as best as I can tell) when we try to call `connRollback`.
The problem is somewhere around here.

Um excuse me *what*? This is also operating in `m (Acquire backend)`, not `Acquire backend`.
How is it possibly being used on the RHS of a `>>=`?

... Oh, right.
Just like `MonadBaseControl IO m => m a` can be concretized to `IO a`, we can concretize `MonadReader r m => m a` to `r -> a`.
So what's happening here is we're picking the spcialized type:

```haskell
rawAcquireSqlConn
    :: Maybe IsolationLevel -> backend -> Acquire backend
```

Wild.

Well, let's look at [`>>=` for `Acquire`](https://www.stackage.org/haddock/lts-17.6/resourcet-1.2.4.2/src/Data.Acquire.Internal.html#line-54):

```haskell
instance Monad Acquire where
    return = pure
    Acquire f >>= g' = Acquire $ \restore -> do
        Allocated x free1 <- f restore
        let Acquire g = g' x
        Allocated y free2 <- g restore `E.onException` free1 ReleaseException
        return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)
```

Hmm!
This smells funny.
The problem occurs when we try to roll back the transaction.
So let's apply some more substitution here.

`Acquire f` contains:

```haskell
\_ -> do
    x <- P.takeResource pool
    pure $ Allocated x (freeConn x)
```

And `g'` contains (simplifying a tiny bit):

```haskell
\sqlBackend -> do
    Acquire $ \_ -> do
        _ <- beginTransaction sqlBackend getter isolation
        pure $ Allocated sqlBackend $ \case
            ReleaseException ->
                connRollback sqlBackend
            _ ->
                connCommit sqlBackend
```

So, we can start inlining.

```haskell
Acquire $ \restore -> do
    Allocated x free1 <- (\_ -> do
        x <- P.takeResource pool
        pure $ Allocated x (freeConn x)) restore

    let Acquire g = g' x

    Allocated y free2 <- g restore `E.onException` free1 ReleaseException
    return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)

-- (\_ -> x) restore = x
Acquire $ \restore -> do
    Allocated x free1 <- do
        x <- P.takeResource pool
        pure $ Allocated x (freeConn x)

    let Acquire g = g' x

    Allocated y free2 <- g restore `E.onException` free1 ReleaseException
    return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)

-- float `c` and `freeConn` up
Acquire $ \restore -> do
    x <- P.takeResource pool
    let free1 = freeConn x

    let Acquire g = g' x

    Allocated y free2 <- g restore `E.onException` free1 ReleaseException
    return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)

-- inline g'
Acquire $ \restore -> do
    x <- P.takeResource pool
    let free1 = freeConn x

    let sqlBackend = x
    let Acquire g = 
            Acquire $ \_ -> do
                _ <- beginTransaction sqlBackend getter isolation
                pure $ Allocated sqlBackend $ \case
                    ReleaseException ->
                        connRollback sqlBackend
                    _ ->
                        connCommit sqlBackend

    Allocated y free2 <- g restore `E.onException` free1 ReleaseException
    return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)

-- Remove `Acquire` constructor:
Acquire $ \restore -> do
    x <- P.takeResource pool
    let free1 = freeConn x

    let sqlBackend = x
    let g _ = do
            _ <- beginTransaction sqlBackend getter isolation
            pure $ Allocated sqlBackend $ \case
                ReleaseException ->
                    connRollback sqlBackend
                _ ->
                    connCommit sqlBackend

    Allocated y free2 <- g restore `E.onException` free1 ReleaseException
    return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)

-- Inline `g`, ignore `restore` parameter
Acquire $ \restore -> do
    x <- P.takeResource pool
    let free1 = freeConn x
    let sqlBackend = x

    Allocated y free2 <-
        (do
            _ <- beginTransaction sqlBackend getter isolation
            pure $ Allocated sqlBackend $ \case
                ReleaseException ->
                    connRollback sqlBackend
                _ ->
                    connCommit sqlBackend
        ) `E.onException` free1 ReleaseException

    return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)
```

Now, this next transformation feels a bit tricky.
I'm going to float `beginTransaction` up and put the `E.onException` only on it.
Note that we're not actually *running* the `free2` action here - just preparing it.
Then I'll assign it with a `let`.

```haskell
Acquire $ \restore -> do
    x <- P.takeResource pool
    let free1 = freeConn x
    let (sqlBackend, localPool) = x

    _ <- beginTransaction sqlBackend getter isolation
        `E.onException` free1 ReleaseException
    let free2 = \case
            ReleaseException ->
                connRollback sqlBackend
            _ ->
                connCommit sqlBackend

    return $! Allocated y (\rt -> free2 rt `E.finally` free1 rt)

-- Inline free1 and free2
Acquire $ \restore -> do
    x <- P.takeResource pool
    let (sqlBackend, localPool) = x

    _ <- beginTransaction sqlBackend getter isolation
        `E.onException` freeConn x ReleaseException

    return $! Allocated y $ \rt -> 
        (\case
            ReleaseException ->
                connRollback sqlBackend
            _ ->
                connCommit sqlBackend) rt 
        `E.finally` 
        (freeConn x rt)

-- Inline freeConn
Acquire $ \restore -> do
    x <- P.takeResource pool
    let (sqlBackend, localPool) = x

    _ <- beginTransaction sqlBackend getter isolation
        `E.onException` 
            P.destroyResource pool localPool sqlBackend

    return $! Allocated y $ \rt -> 
        (case rt of
            ReleaseException ->
                connRollback sqlBackend
            _ ->
                connCommit sqlBackend)
        `E.finally` do
            case rt of
                ReleaseException -> 
                    P.destroyResource pool localPool sqlBackend
                _ -> 
                    P.putResource localPool sqlBackend
```

I think it's important to note that, again we *don't ever actually call `restore`*.
So the masking state is inherited and not ever changed.
It feels important but I'm not sure if it actually is.

Let's plug *this* into `withAcquire` now.

```haskell
withAcquire (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    Allocated x free <- f restore
    res <- restore (run (g x)) `E.onException` free ReleaseException
    free ReleaseNormal
    return res

-- Inline `f`. Since `restore` is never called, we can omit passing it as 
-- a parameter.
withAcquire (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    Allocated x free <- do
        x <- P.takeResource pool
        let (sqlBackend, localPool) = x

        _ <- beginTransaction sqlBackend getter isolation
            `E.onException` free1 ReleaseException

        return $! Allocated x $ \rt -> 
            (case rt of
                ReleaseException ->
                    connRollback sqlBackend
                _ ->
                    connCommit sqlBackend)
            `E.finally` do
                case rt of
                    ReleaseException -> 
                        P.destroyResource pool localPool sqlBackend
                    _ -> 
                        P.putResource localPool sqlBackend

    res <- restore (run (g x)) `E.onException` free ReleaseException
    free ReleaseNormal
    return res

-- float `x <- P.takeResource pool` to the top, and define `free` using `let`
withAcquire (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    x <- P.takeResource pool
    let free1 = freeConn x
    let (sqlBackend, localPool) = x

    _ <- beginTransaction sqlBackend getter isolation
        `E.onException` free1 ReleaseException

    let free rt = 
            (case rt of
                ReleaseException ->
                    connRollback sqlBackend
                _ ->
                    connCommit sqlBackend)
            `E.finally` do
                case rt of
                    ReleaseException -> 
                        P.destroyResource pool localPool sqlBackend
                    _ -> 
                        P.putResource localPool sqlBackend

    res <- restore (run (g x)) `E.onException` free ReleaseException
    free ReleaseNormal
    return res

-- inline `free` for each case:
withAcquire (Acquire f) g = withRunInIO $ \run -> E.mask $ \restore -> do
    x <- P.takeResource pool
    let (sqlBackend, localPool) = x

    _ <- beginTransaction sqlBackend getter isolation
        `E.onException` 
            P.destroyResource pool localPool sqlBackend

    res <- restore (run (g x)) `E.onException` do
            connRollback sqlBackend
                `E.finally`
                    P.destroyResource pool localPool sqlBackend

    do -- ReleaseNormal
        connCommit sqlBackend
            `E.finally` do
                P.putResource localPool sqlBackend
    return res
```

Let's consider our masking state.
We're masked for *everything* except for the `restoure (run (g x))` call.
Including beginning the transaction and committing the transaction.

But we can still receive asynchronous exceptions during [interruptible operations](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception.html#interruptible).
Interruptible operations include "anything that can block or perform IO," which seems very likely to include the Postgres code here.

# The Original

Let's compare this with the original code.
The [original code](https://github.com/yesodweb/persistent/pull/984/files#diff-f9d7f232cd00cb88188b7fcc68110e3f4cb378fcad9df652360de44d13cd86e3L31) delegated to [`runSqlConn`](https://github.com/yesodweb/persistent/pull/984/files#diff-f9d7f232cd00cb88188b7fcc68110e3f4cb378fcad9df652360de44d13cd86e3L63-L72) after acquiring a `SqlBackend` from the `Pool` in `MonadUnliftIO`.

```haskell
runSqlConn :: (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> backend -> m a
runSqlConn r conn = withRunInIO $ \runInIO -> mask $ \restore -> do
    let conn' = projectBackend conn
        getter = getStmtConn conn'
    restore $ connBegin conn' getter Nothing
    x <- onException
            (restore $ runInIO $ runReaderT r conn)
            (restore $ connRollback conn' getter)
    restore $ connCommit conn' getter
    return x
```

We'll inline this into `runSqlPool`, so we'll now see:

```haskell
runSqlPool r pconn = 
    withRunInIO $ \run -> 
    withResource pconn $ run . runSqlConn r

-- expand lambda
runSqlPool r pconn = 
    withRunInIO $ \run -> 
    withResource pconn $ \conn -> 
    run $ runSqlConn r conn

-- inline runSqlConn
runSqlPool r pconn = 
    withRunInIO $ \run -> 
    withResource pconn $ \conn -> 
    run $ withRunInIO $ \runInIO -> 
    mask $ \restore -> do
        let conn' = projectBackend conn
            getter = getStmtConn conn'
        restore $ connBegin conn' getter Nothing
        x <- onException
                (restore $ runInIO $ runReaderT r conn)
                (restore $ connRollback conn' getter)
        restore $ connCommit conn' getter
        return x
```

Kind of a lot of `withStuff` going on, including two `withRunInIO`s lol.
Let's make it even worse by inlining `withResource`:

```haskell
-- abstract action to a variable
runSqlPool r pconn = 
    withRunInIO $ \run -> 
    withResource pconn $ \conn -> 
    let act = 
            run $ withRunInIO $ \runInIO -> 
            mask $ \restore -> do
                let conn' = projectBackend conn
                    getter = getStmtConn conn'
                restore $ connBegin conn' getter Nothing
                x <- onException
                        (restore $ runInIO $ runReaderT r conn)
                        (restore $ connRollback conn' getter)
                restore $ connCommit conn' getter
                return x
     in act

-- inline withResource

runSqlPool r pconn =
    withRunInIO $ \run -> 
    -- withResource pconn $ \conn ->
    control $ \runInIO0 -> 
    mask $ \restore0 -> do
        let act conn = 
                run $ withRunInIO $ \runInIO1 -> 
                mask $ \restore1 -> do
                    let conn' = projectBackend conn
                        getter = getStmtConn conn'
                    restore1 $ connBegin conn' getter Nothing
                    x <- onException
                            (restore1 $ runInIO1 $ runReaderT r conn)
                            (restore1 $ connRollback conn' getter)
                    restore1 $ connCommit conn' getter
                    return x
        (resource, local) <- takeResource pool
        ret <- restore0 (runInIO0 (act resource)) `onException`
                  destroyResource pool local resource
        putResource local resource
        return ret

-- inline `act`
runSqlPool r pconn =
    withRunInIO $ \run -> 
    -- withResource pconn $ \conn ->
    control $ \runInIO0 -> 
    mask $ \restore0 -> do
        (resource, local) <- takeResource pool
        ret <- restore0 (runInIO0 (
            run $ withRunInIO $ \runInIO1 -> 
            mask $ \restore1 -> do
                let conn = resource
                let conn' = projectBackend conn
                    getter = getStmtConn conn'
                restore1 $ connBegin conn' getter Nothing
                x <- onException
                        (restore1 $ runInIO1 $ runReaderT r conn)
                        (restore1 $ connRollback conn' getter)
                restore1 $ connCommit conn' getter
                return x)) `onException`
                  destroyResource pool local resource
        putResource local resource
        return ret
```

The `restore` paratmer in `mask` doesn't unmask it completely - it restores the existing `mask`ing state before the `mask` was entered.
So `mask $ \restore -> mask $ \restore -> restore (print 10)` doesn't have `print 10` in an *unmasked* state, but the same mask as before.
However, here, we have this pattern:

```haskell
mask $ \restore -> do
    restore $ do
        mask $ \restore' -> do
            ...
```

Which is interesting!

```haskell
runSqlPool r pconn =
    -- Unmasked
    withRunInIO $ \run -> 
    control $ \runInIO0 -> 
    mask $ \restore0 -> do
        -- Masked
        (resource, local) <- takeResource pool
        ret <- restore0 
            -- Unmasked
            (runInIO0 $ run $ withRunInIO $ \runInIO1 ->
            -- Masked
            mask $ \restore1 -> do
                let conn = resource
                let conn' = projectBackend conn
                    getter = getStmtConn conn'
                -- Unmasked
                restore1 $ connBegin conn' getter Nothing
                x <- onException
                                    -- Unmasked
                        (restore1 $ runInIO1 $ runReaderT r conn)
                                    -- Unmasked
                        (restore1 $ connRollback conn' getter)
                restore1 $ do --unmasked
                    connCommit conn' getter
                return x)
                -- Masked
                `onException`
                  destroyResource pool local resource
        -- Still masked
        putResource local resource
        return ret
```

So our masked actions are:

1. `takeResource pool`
2. `onException`
3. `onException` and then `destroyResource`
4. `putResource`

Unmasked, we have:

1. `connBegin`
2. `r` (the action passed to `runSqlConn`)
3. `connRollback`
4. `connCommit`

Let's compare with `withAcquire` which was all inlined above:

* Masked:
    1. `takeResource`
    2. `beginTransaction`
    3. `destroyResource`
    4. `connRollback`
    5. `destroyResource` again
    6. `connCommit`
    7. `putResource`
* Unmasked
    1. `run (g x)` -- the action passed to `withAcquire` and `runSqlConn`.

So `withAcquire` actually has quite a bit more masking going on!
Interesting.
Remembering, the problem occurs when the `thread killed` exception happens and the `connRollback` function is called, causing `libpq` to die with the "command in progress" error.

So, we throw a `killThread` at our `withAcquire` function.
It'll land as soon as we're unmasked, or an interruptible action occurs.
Since almost all of it is masked, we need to determine what the interruptible operations are.

[`takeResource`](https://www.stackage.org/haddock/lts-17.6/resource-pool-0.2.3.2/src/Data.Pool.html#takeResource) might be interruptible - it has an STM transaction, which does call `retry`.
I don't know if *any* code with `retry` triggers an interrupt, or if only actually *calling* `retry` can trigger an interruptible state.
Based on a quick and bewildering look at the GHC source, I think it's just that `retry` itself can be interrupted.
`retry` occurs when there are no available entries in the local pool *and* we're at max resources for the pool.
This is exactly the scenario this test is exercising: a single stripe with a single resource that's constantly in use.

`beginTransaction` kicks off an IO action to postgres, so it is almost definitely interruptible.
Same for `connRollback` and `connCommit`.
So the masked-state for these items in `withAcquire` is *probably* not a big deal - but we could check by using `uninterruptibleMask` on them.

# To be continued?

I wish I had a more satisfying conclusion here, but I'm all out of time to write on this for now.
Please comment on the relevant GitHub issues if you're interested or have some insight!
