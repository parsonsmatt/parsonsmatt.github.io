---
title: "Scotty and Persistent"
date: 2015-05-02
layout: post
categories: programming
---

## A beginner's voyage

(this is part one of three: [two](/programming/2015/05/04/perscotty_pt_ii.html) and [three](/programming/2015/05/10/perscotty_pt_iii.html) are linked)

I've been working on a small application with the Haskell web framework scotty, and decided to use the package Persistent to provide database access. I had some trouble getting them to work together, and I couldn't find many complete examples that used PostgreSQL. I thought I'd put at least one example online of how I've gotten it to work thus far.

Actually, I lied. I haven't gotten it to work yet. This blog post is as much a rubber-ducky discovery process as it is a guide. If you have any complaints, suggestions, comments, or questions, I'd love to hear them.

I'm going to be tagging commits in a repository, so you'll have full code examples to work with. The repository for the first part is [here](https://www.github.com/parsonsmatt/scotty-persistent-example).

## Smoke Test: Just the DB

The [Yesod Book's Persistent chapter](http://www.yesodweb.com/book/persistent) is very good, and has a great starting point for getting it working. The following code snippet is from the Synopsis, and it's what we'll be using to make sure that everything is working with Persistent before worrying too much about integrating with scotty.

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]
```

First thing's first: make a directory to test this, `cabal init` to make a project, and `cabal sandbox init` to avoid screwing up my global package directory. I add `persistent, persistent-template, persistent-sqlite` to my `build-depends`, and do `cabal run`. I get an error that `Could not find module 'Control.Monad.IO.class'...`, so I add the `transformers` package to build-depends. `cabal run` now works without errors! Hooray! Now let's get it running with Postgres.

This section is tagged [sqlite](https://github.com/parsonsmatt/scotty-persistent-example/tree/sqlite) in the repository.

### To PostgreSQL and Beyond!

First, we need to change the packages to PostgreSQL instead of SQLite. Change the line in `build-depends` to require `persistent-postgresql` instead of `-sqlite`, and change the import line in `Main.hs` to `import Database.Persist.Postgresql`. `cabal install --dep` (my favorite shorthand for the otherwise verbose `--dependencies-only`) to get them installed. Now running the project gets the highly uninteresting error "Not in scope: `runSqlite`". The bottom of that Yesod Book post indicates what we need in order to replace the `runSqlite` function:

```haskell
... -- In the imports, add:
import           Control.Monad.Logger    (runStderrLoggingT)
...

connStr = "host=localhost dbname=test user=test password=test port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
        flip runSqlPersistMPool pool $ do
            runMigration migrateAll

        -- etc ...
```

Now, the module complains about lacking the `Control.Monad.Logger` module. Add it to `build-depends`. Let's try running now! `cabal run` and after a lot of thinking, it spits out:

> libpq: failed (could not connect to server: Connection refused. Is the server running on host "localhost" and accepting TCP/IP connections on port 5432?

I honestly wasn't expecting that -- I was sure it'd give an authentication error! `service postgresql status` informs me that Postgres is running on port 5433 for some reason, so I edit the ConnectionString. Now I'm getting the expected problem: `FATAL: password authentication failed for user "test"`. This isn't surprising, as I've not made a test user, test database, or test password. I make a Postgres user with `createuser -s test -W` (warning: this is terrible insecure! Use a more secure means of authentication for your actual application), make a database with `createdb perscotty`, and modify the `connStr` to reflect this:

```haskell
...
connStr = "host=localhost dbname=perscotty port=5433 user=test password=test"
...
```

I had to enter psql and manually do an `ALTER ROLE test WITH PASSWORD 'test';`, but afterwards, it all worked!

So, what does all of that new code do, anyway? Let's check out [the Hackage page!](https://hackage.haskell.org/package/persistent-postgresql-2.1.5/docs/Database-Persist-Postgresql.html). Actually I kind of have no idea what that does. The rest of this is going to be a bit of an adventure.

The repository at this point has the [postgres](https://github.com/parsonsmatt/scotty-persistent-example/tree/postgres) tag.

### Breaking it down

Now, this can't all be like this. It's totally not a fun web application, and that's what I signed up for. Let's break it up into usable functions. First, ghc-mod is whining about some linting, which I'll go ahead and follow. Redundant do and discarded values. Next I'll rename `main` to `dbFunction` and make a new `main` that just calls that. Let's pull out the `runMigration` line too, and also the database inserts/deletes into their own functions. ghc-mod will complain about some *crazy* missing type signatures, but that's fine, really, I don't mind missing those at the moment (I need to learn monad stacks). The current state looks something like:

```haskell
dbFunction = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
        flip runSqlPersistMPool pool $ do
            doMigrations
            doDbStuff
```

Neat! I feel like we're very close to a workable solution. We can refactor this to use parameters, and rewrite the current code as:

```haskell
main :: IO ()
main = do
    dbFunction doMigrations
    dbFunction doDbStuff

dbFunction query = runStderrLoggingT $ 
        withPostgresqlPool connStr 10 $ 
        \pool -> liftIO $ runSqlPersistMPool query pool

doMigrations = runMigration migrateAll

doDbStuff = do
        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing
        -- ...
```

Alright, neat, we've broken the functions up, and can now user Persistent pretty well. Hooray! The code at this point is tagged at [break-it-up](https://github.com/parsonsmatt/scotty-persistent-example/tree/break-it-up).

## Beam Me Up, Scotty

Now, for the exciting stuff -- let's make this all work with scotty and expose it to the web! Add scotty and wai-extra to the build-depends and install them. Import qualified Scotty and the wai middleware request logger, and we can make our app a website. Our main function looks like this now:

```haskell
main = do
    dbFunction doMigrations
    dbFunction doDbStuff
    S.scotty 3000 $ do
        S.middleware logStdoutDev
        S.get "/" $ S.html "Hello World"
```
I'd really like to get that db stuff *in* the app, instead of before it. After all, we'll be needing to do all of our database stuff inside of scotty request handlers. The magic function for that is `liftIO`! So now we can move the DB migration and STUFF into the scotty app. I'll also define a function to make it a bit less verbose. Now we're looking like:

```haskell
main = S.scotty 3000 $ do
        S.middleware logStdoutDev
        inAppDb $ do 
            doMigrations
            doDbStuff
        S.get "/" $ S.html "Hello World"
```
Nice! We're getting somewhere!

### One level deeper...

Let's do some DB stuff in the handler action! This turns out to be a bit more complex, unfortunately... Here is the revised code that 'works':

```haskell
main :: IO ()
main = S.scotty 3000 $ do
        S.middleware logStdoutDev
        inAppDb $ do 
            doMigrations
            doDbStuff
        S.get "/" $ S.html "Hello World"
        S.get "/posts" $ do
            posts <- inHandlerDb $ selectList [] []
            S.html ("Posts!" <> (T.pack $ show $ length (posts :: [Entity BlogPost])))
        S.get "/posts/:id" $ do
            postId <- S.param "id"
            findPost <- inHandlerDb $ get (toSqlKey (read postId))
            S.html $ "You requested post: <br>" <> (T.pack $ show (findPost :: Maybe BlogPost))

inHandlerDb = liftIO . dbFunction
inAppDb = liftIO . dbFunction
```

Haskell's inferred type for inHandlerDb is `forall a. SqlPersistM a -> Web.Scotty.Internal.Types.ActionT T.Text IO a` and for inAppDb is `forall a. SqlPersistM a -> Web.Scotty.Internal.Types.ScottyT T.Text IO a`. I've no idea what the `forall` bit is about, and I'm sure there's a better way to handle this than making two type-inferred functions, but that's why this is a learning process, right?

So, this is working! It's querying the database, returning the count of the posts on that index action and returning a `Maybe BlogPost` when you request a given ID. All of the pieces are here to make a much, *much* nicer solution.

The final version is tagged [scotty](https://github.com/parsonsmatt/scotty-persistent-example/tree/scotty).

## Pain Points

This wasn't an easy process by any means. Reading the available material was good to get me started, but even incredibly basic querying like "How do I get all records of a type out of the database?" or "How do I get a record of a given type with a certain ID?" is non-obvious to a nooblet like myself. 

First, you have to convert an integer into Persistent's `Key` type. `toSqlKey` handles that nicely. Next, you annotate the type of what you're looking for. That's how Persistent figures out which table to query.

```haskell
findPost <- inHandlerDb $ get (toSqlKey (read postId))
... (findPost :: Maybe BlogPost)
```

This also works for selecting multiple records. The code above that gets all posts out of the database is:

```haskell
posts <- inHandlerDb $ selectList [] []
... (posts :: [Entity BlogPost]) ...
```

This is pretty awesome. But it's extremely difficult to search for implied information like that, and it took me a really long time to figure that out. It appears that `ScopedTypeVariables` extension would allow one to write `posts :: [Entity BlogPost] <- inHandlerDb $ selectList [] []`, which is an extremely nice syntax for what's happening here.

I'll let this be part #1, and I'll write another post on cleaning all of this up and applying it in a real(ish) app. If you have any questions, corrections, or comments, please feel free to [email me!](mailto:parsonsmatt@gmail.com)
