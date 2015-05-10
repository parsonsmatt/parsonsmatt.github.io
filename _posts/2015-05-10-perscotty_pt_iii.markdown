---
title: "Perscotty Pt III"
date: 2015-05-10
layout: post
categories: programming
---

# Finishing the Story

Last time, we corrected resource utilization by pulling the pool out and reusing it in queries rather than creating and destroying it each time. We also setup the app to run in a specialized monad transformer stack, allowing us some read-only configuration information, but we had to seriously trim down our application in order to figure out how to get that working. Let's restore our original functionality!

## Reading the Database

First, let's do up our index action for all the posts. `S.get "/posts" (html "listing all posts!")` gets added to the application. Let's figure out how to use that database pool, tucked away in the Reader monad. Having learned my lesson about building up big complex things before breaking them into smaller bits, I want to stop inlining the actions in these and start using real functions. So let's make the handler for `postsIndex` it's own function. ghc-mod helpfully tells us that the type of `(html "listing all posts!")` is `ActionT T.Text ConfigM ()`, so let's make our function and call it.

```haskell
app = do
    middleware logStdoutDev
    S.get "/" (html "hello world")
    S.get "/posts" postsIndex

postsIndex :: ActionT T.Text ConfigM ()
postsIndex = html "listing all posts!"
```
Much cleaner. Now, what will our code look like to access the database? Our earlier function for database access was this:

```haskell
runDb pool query = liftIO (runSqlPool query pool)
```
Now that we can read the pool from our ConfigM, we don't have to pass it explicitly as a parameter. So let's rewrite the function to get the pool from the reader. The function to get things out of the Reader is `asks`. Our `Reader` environment is a value of type `Config { getPool :: ConnectionPool }`, so the way to extract the pool from that is `getPool cfg`. So we'll `asks getPool`. A first attempt!

```haskell
runDb query = do
   pool <- asks getPool
   liftIO (runSqlPool query pool)
```
Now, there's something wrong with this pool. We're not in the Reader monad, we're in the ActionT monad, and the Reader monad is below our current context. So we have to `lift` that `asks` function up into the current monad before it'll work. As is, we get an error. Let's fix it up!

```haskell
runDb query = do
    pool <- lift $ asks getPool
    liftIO (runSqlPool query pool)
```

And this works! We'll update the `postsIndex` to make use of this. We'll just get a count of posts in the database.

```haskell
postsIndex :: ActionT T.Text ConfigM ()
postsIndex = do
    posts <- runDb (selectList [] [])
    html $ "This many posts! <br>" <> T.pack (show (length (posts :: [Entity BlogPost])))
```

We can fire up the server, run it, and AH HAH! It works! It's correctly reporting the count of posts in the database. Fantastic.

## But does it scale?

Previously, we'd use a function somewhere, and that would 'bake in' the type. Will this new `runDb` function work in other contexts? We'll uncomment the `doMigrations` and `doDbStuff` functions, run them in the app, and see what happens.

```haskell
app = do
    runDb doMigrations
    runDb doDbStuff
    -- ...
```

And it works! So our runDb function appears to be OK with working in any level of the stack, as long as it's got that `ConfigM` to read from. ghc-mod infers the following type for `runDb`:

```haskell
runDb :: forall b (t :: (* -> *) -> * -> *) (m :: * -> *). 
         (MonadTrans t, MonadReader Config m, MonadIO (t m)) 
         => SqlPersistT IO b -> t m b
```
Which is a little more type sorcery than I am comfortable with. Taylor's example has the following type:

```haskell
runDb :: (MonadTrans t, MonadIO (t ConfigM))
         => SqlPersistT IO a -> t ConfigM a
```
Instead of accepting any `MonadReader Config m`, we only want to accept `ConfigM`. `ConfigM` is an instance of `MonadReader Config` already, as derived in the newtype declaration. It turns out, that's all that needs to be done, and now we have a working database connection that we can easily use in our application.

If we want to make additional information available to our application, all we have to do is add another field to the ConfigM type and set that up top. That's rather nice to work with! You can get the current state of the repository [here](https://github.com/parsonsmatt/scotty-persistent-example/tree/finished). Many thanks to Taylor Fausak for the excellent [Building a JSON REST API in Haskell](http://taylor.fausak.me/2014/10/21/building-a-json-rest-api-in-haskell/) post.
