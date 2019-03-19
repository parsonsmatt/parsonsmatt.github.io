---
title: "Implementing UNION in Esqueleto II"
date: 2019-02-01
layout: post
categories: programming
---

We're trying to implement the SQL `UNION` function in the `esqueleto` database library.
Last time, I started off with some syntax I liked, and reached an implementation.
Unfortunately, that implementation did not work like I wanted it to!
Let's figure out a way to get it working, or possibly another approach.

This is the second post in the series.
[Click here for part I]({% post_url 2019-01-31-esqueleto_union_i %}).

# Hacking it together

The problem I ran into last time is that this Haskell code:

```haskell
select $
  (from $ \person -> pure (person ^. PersonName))
  `union`
  (from $ \blog -> pure (blog ^. BlogPostTitle))
```

was producing the following SQL code:

```sql
SELECT
  ( SELECT name FROM person
    UNION
    SELECT title FROM blog_post
  )
```

which produces only a single result.

So, what is `select` doing?
Let's investigate the definition a bit:

```haskell
select :: ( SqlSelect a r
          , MonadIO m
          )
       => SqlQuery a -> SqlReadT m [r]
select query = do
    res <- rawSelectSource SELECT query
    conn <- R.ask
    liftIO $ with res $ flip R.runReaderT conn . runSource
```

It delegates to `rawSelectSource` for the actual work.
Let's get that definition (comments mine):

```haskell
rawSelectSource 
  :: (SqlSelect a r , MonadIO m1 , MonadIO m2)
  => Mode
  -> SqlQuery a
  -> SqlReadT m1 (Acquire (C.ConduitT () r m2 ()))
rawSelectSource mode query = do
  -- [1]
  conn <- projectBackend <$> R.ask
  let _ = conn :: SqlBackend
  res <- R.withReaderT (const conn) (run conn)
  return $ (C..| massage) `fmap` res
  where
    -- [2]
    run conn =
      -- [3]
      uncurry rawQueryRes $
      first builderToText $
      -- [4]
      toRawSql mode (conn, initialIdentState) query

    massage = do
      mrow <- C.await
      case process <$> mrow of
        Just (Right r)  -> C.yield r >> massage
        Just (Left err) -> liftIO $ throwIO $ PersistMarshalError err
        Nothing         -> return ()

    process = sqlSelectProcessRow
```

This code has a somewhat odd style to it.

1. I'd probably write this as `R.asks projectBackend`, and instead of the `let _ = conn :: SqlBackend`, enable `ScopedTypeVariables` so I could write:

    ```haskell
      conn :: SqlBackend <- R.asks projectBackend
    ```

    But this is a minor complaint.
2. This seems to be the meat of this function.
3. [`rawQueryRes`](https://www.stackage.org/haddock/lts-13.5/persistent-2.9.1/Database-Persist-Sql.html#v:rawQueryRes) is a function from `persistent`. It accepts a query, a list of parameters to substitute into the query, and runs the query against the database and returns a streaming `Source` of results.
4. `toRawSql` is a function that converts an `esqueleto` `SqlQuery a` into the textual query as well as a list of parameter substitutions.

`toRawSql` is the function that actually constructs the query.
We could make a quick hack, and redefine `union` to instead run the query directly, so we'd write:

```haskell
res <-
  (from $ \p -> pure (p ^. PersonName))
  `union`
  (from $ \b -> pure (b ^> BlogPostTitle))
```

But then we can't compose queries.
That's not satisfactory!

So instead, we'll write a replacement for `select` that works with `union`, and we'll call it `runUnion`.
