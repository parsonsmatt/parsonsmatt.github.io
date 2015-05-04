---
title: "Perscotty Pt II"
date: 2015-05-04
layout: post
categories: programming
---

## Some updates

The [reddit thread](https://www.reddit.com/r/haskell/comments/34nxtu/scotty_and_persistent_a_beginners_voyage/) about my [previous post](http://www.parsonsmatt.org/programming/2015/05/02/scotty_and_persistent.html) generated good discussion and advice. I'm going to attempt to work through them now.

## Pooling

The most urgent issue is that my database pool is getting recreated every time I make a query, and then closed. Noo! Instead, I need to create a pool, and pass that to the database functions so they can efficiently reuse the resources. Also, it's not really necessary to run the migrations from within the server, so we'll extract that out. I'm also going to unqualify the scotty import to make the code a bit more readable.

```haskell
runDb query pool = liftIO (runSqlPool query pool)

main :: IO ()
main = do
    pool <- runStdErrLoggingT $ createPostgresqlPool connStr 10
    runDb doMigrations pool
    rubDb doDbStuff pool
    scotty 3000 $ do -- ...
```
Pool acquired! Now we can delete the `inAppDb` function. Let's get the inHandlerDb stuff working too. I'll just dumb replace the inHandlerDb call with runDb and add the pool parameter:

```haskell
-- old:
posts <- inHandlerDb (selectList [] [])
-- new:
posts <- runDb (selectList[] []) pool
```
And it works! I was kind of expecting a type mismatch that would require another function to be made, but this didn't. Let's inspect the inferred types?

```haskell
inAppDb :: SqlPersistM a -> ScottyT T.Text IO a
inHandlerDb :: SqlPersistM a -> ActionT T.Text IO a
runDb :: MonadIO m => SqlPersistT IO a -> Pool SqlBackend -> m a
```

It looks like the reason that `runDb` is more general is because the inferred type doesn't restrict it to a given monad, and it is expecting a transformer `SqlPersistT` instead of the `SqlPersistM`.

## Fat Stacks of Monads

Passing around the `pool` is kind of annoying. Let's make a helper function that will encapsulate that process.

```haskell
main = do
    pool <- etc...
    let runDb' = runDb pool
    runDb' doMigrations
    ...
```
This works, at first! Unfortunately, it doesn't work across different monad stacks. The types don't line up. So we need to figure out how to pass that around without screwing anything up.
