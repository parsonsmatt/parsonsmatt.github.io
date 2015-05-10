---
title: "Perscotty Pt II"
date: 2015-05-04
layout: post
categories: programming
---

## Some updates

The [reddit thread](https://www.reddit.com/r/haskell/comments/34nxtu/scotty_and_persistent_a_beginners_voyage/) about my [previous post](http://www.parsonsmatt.org/programming/2015/05/02/scotty_and_persistent.html) generated good discussion and advice. I'm going to attempt to work through them now.

[This guide](http://taylor.fausak.me/2014/10/21/building-a-json-rest-api-in-haskell/) contains a lot of information on what this all should look like in the end. I'll be taking a decent bit of information from that.

## Pooling

The most urgent issue is that my database pool is getting recreated every time I make a query, and then closed. Noo! Instead, I need to create a pool, and pass that to the database functions so they can efficiently reuse the resources. Also, it's not really necessary to run the migrations from within the server, so we'll extract that out. I'm also going to unqualify the scotty import to make the code a bit more readable.

```haskell
runDb pool query = liftIO (runSqlPool query pool)

main :: IO ()
main = do
    pool <- runStdErrLoggingT $ createPostgresqlPool connStr 10
    runDb pool doMigrations 
    rubDb pool doDbStuff
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

Passing around the `pool` is kind of annoying, especially when the application gets more complex. Let's try to make a helper function that will encapsulate that process.

```haskell
main = do
    pool <- etc...
    let runDb' = runDb pool
    runDb' doMigrations
    ...
```
This works, at first! Unfortunately, it only works when we keep it in the top level. It doesn't let us use this function in the application. The types don't line up. Experiment with it a bit: the first place that you *use* the `runDb'` function is what coerces the type, and the type of function to run the database inside the application is incompatible with the type of function to run the database outside of the database. Let's use `ghc-mod` to inspect the inferred type of `runDb'` in the above context:

```haskell
SQLPersistT IO -> IO ()
```
And the type of `runDb pool` in the above context:

```haskell
SqlPersistT [Entity Blogpost] -> Web...ActionT T.Text IO [Entity Blogpost]
```
In SQLPersistT and ActionT, the T indicates that these are monad transformers. A monad transformer gets stacked on top of another monad, allowing you to access two monads. And monad transformers are themselves monads, so you can stack as many as you want! So we want to somehow generalize `SqlPersistT [Entity Blogpost] -> ActionT Text IO [Entity Blogpost]`. I tried a number of possible avenues for that, but wasn't able to derive a function that would work generically.

## Reader

The Haskell idiom for implicitly threading some read-only information throughout a program is the Reader monad. The JSON API linked above uses this, along with a Config data type, to build the application up. Let's start small and build something similar. Let's keep the `Config` data type small, and just store the connection pool for now:

```haskell
data Config = Config { getPool :: ConnectionPool }
```
Next up is defining the `Reader` monad for this. The bottom of the stack is `IO`, so our `Reader` will read from Config and sit on top of `IO`. Here's the code (pulled from Taylor Fausak's post):

```haskell
newtype ConfigM a = ConfigM 
    { runConfigM :: ReaderT Config IO a
    } deriving (Applicative, Functor, Monad, 
                MonadIO, MonadReader Config)
```

We had to add a few imports up top to get this to work, and add the `mtl` library to the cabal file. Alright! How do we actually use this thing? It turns out, we need to stop calling `scotty` and call something else entirely. Taylor's guide calls `scottyOptsT` which has a pretty full configuration set. For right now, I'd like to keep it a bit simpler. Let's explore the [Hackage documentation](https://hackage.haskell.org/package/scotty-0.9.1/docs/Web-Scotty-Trans.html) for scotty's types and see what we can do. `scottyT` looks like the simplest of the bunch, so let's run with that.

Before we get too crazy, let's make sure we can get `scottyT` working just by passing `id` in. Our main function now reads:

```haskell
main = do
    pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
    runDb pool migrations
    runDb pool doDbStuff
    scottyT 3000 id id $ do
      -- ...
```
This works! Awesome! So, what's going on with those `id`s there? Inspecting the type with ghc-mod gives us `IO a -> IO a` for the first one, and `IO Response -> IO Response`. Cool. Let's add a line `let c = Config pool` under the pool declaration to make our Config data. Taylor's guide has the following function, which I'm going to copy in: `let r m = runReaderT (runConfigM m) c`. The text about that function reads:

> This takes Scotty’s monad m and adds the ability to read our custom config c from it. This is called a monad transformer stack. It allows us to use any monad in the stack. So after adding our reader monad, we can both deal with requests (using Scotty’s monad) and read our config (using our monad).

Cool! Let's change `id` to `r` in both of those and see what happens...

## BOOM

Type errors! Type errors everywhere! While looking at the hackage documentation above, I noticed that all of the normal methods were duplicated in the Web.Scotty.Trans package. Let's swap out the Scotty version of those functions with the ScottyT versions. And now we're getting entirely different type errors! We're missing an instance for `ScottyError`. So let's break the application code into it's own function, give that a type signature, and see what happens.

Now we're doing `scottyT 3000 r r application` and defining `application` below. The `pool` went out of scope. Let's just be a tiny bit lazy, and comment out the whole body of that function, and just do a basic `"hello world"` for now. We want to get the monad stack working, and database access will be easy as pie after that. Here's what our main function and app functions look like now:

```haskell
main = do
    pool <- runStdOutLogggingT $ createPostgresqlPool connStr 10
    let cfg = Config pool
        r m = runReaderT (runConfigM m) cfg
    scottyT 3000 r r app

app :: ScottyT T.Text ConfigM ()
app = S.get "/" $ S.html "Hello world"
```
Now we're getting a new and entirely vexing type error:

> Couldn't match type ‘a’ with Response’ 
>
> ‘a’ is a rigid type variable bound by a type expected by the context: 
>
> ConfigM a -> IO a at Main.hs:60:5
>
> Expected type: ConfigM a -> IO a 
> Actual type: ConfigM Response -> IO Response
>
> In the second argument of ‘scottyT’, namely ‘r’
>
> In a stmt of a 'do' block: scottyT 3000 r r app

(the actual error said "wai-3.0.2.3:Network.Wai.Internal.Response", and I trimmed it for readability)

## A detour

I dug around the internet for *hours* trying to find the solution to this, and I never really got there. I tried so many things, and nothing fixed it. Finally, I decided I'd rip out as much code as possible for a minimal reproduction to be able to ask the Internet, and something magical happened...

First, I deleted all the code except for stuff directly required for the `main` and `app` functions above. `ghc-mod` let me know about a bunch of unused imports, so I trimmed the import list down until it was *the bare necessities*. ghc-mod was kind enough to let me know that I had a bunch of unused language pragmas, so I removed them. At this point, the type error goes away, and everything works. *What*. What. I don't even know. I re-add them, problem recurs. I remove them one-by-one, and the problem was evidently with the `GADTs` language pragma, which was used by Persistent's Template Haskell implementation. Weird. Let this be a warning -- break your code into modules, and localize things as much as possible!

### Break out the data model

`touch Model.hs`, throw all the Persistent stuff in there (minus the stuff required for `ConnectionPool`, etc) and start taking language pragmas out of Main. This time, GADTs didn't fix it, but TypeFamilies did. Well, whatever. Our minimal HelloWorld with the right monad is finally working.

The repository in the current state is available [here](https://github.com/parsonsmatt/scotty-persistent-example/tree/monad-stacks). You can look through the commit history and see the incremental changes that I made. This blog post is already way longer than is necessary or normal, so I'll actually get the little demo working with the database next time.
