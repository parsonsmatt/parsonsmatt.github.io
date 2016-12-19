---
title: "Servant in Yesod - Yo Dawg"
date: 2016-12-18
layout: post
categories: programming
---

If you're a web programmer, Haskell has a lot of neat toys.
[Servant](http://haskell-servant.readthedocs.io/en/stable/) does a fantastic job of describing RESTful APIs, and the ability to generate [JavaScript clients](http://haskell-servant.readthedocs.io/en/stable/tutorial/Javascript.html), [Haskell clients](http://haskell-servant.readthedocs.io/en/stable/tutorial/Client.html), and [Swagger documentation and UIs](https://haskell-servant.github.io/posts/2016-02-06-servant-swagger.html) make it a compelling choice for implementing your API.
[Yesod](http://www.yesodweb.com/) makes a similarly compelling choice for full blown websites, with lots of documentation, HTML templates, solid routing and type safe links, and convenient database modeling.
Where Servant excels for RESTful APIs, Yesod excels for websites.

If you're writing a web app, you may be wondering: how do I choose?
An API may end up needing to render some pages, and a website may need to expose JSON endpoints.
Fortunately, we can easily have both!
In this blog post, I'll demonstrate how to mount my [servant-persistent](https://github.com/parsonsmatt/servant-persistent) starter project inside of a newly minted Yesod application.

tl;dr: Both Servant and Yesod expose functions to convert them to [WAI](https://hackage.haskell.org/package/wai) applications, and both have means of running arbitrary `WAI` applications.
If you're too impatient to read the walkthrough, the complete repository is [on Github](https://github.com/parsonsmatt/yo-dawg).

# `servant-persistent`

Since we'll be using this package as our API, you may want to read a bit about it. 
I wrote a post describing the project and how it's used [here](http://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html).

# Start your Yesods

Yesod has a feature called [subsites](http://www.yesodweb.com/book/creating-a-subsite).
This allows you to write a modular little website, and then put it inside of a larger site.
A lesser known feature is the `WaiSubsite` which allows you to embed an arbitrary `WAI` `Application`.
We'll use this feature to embed the Servant app.

Start up a new Yesod project using `stack`, like so:

```
stack new yo-dawg yesod-postgres --resolver=lts-6.27
```

Next up, we'll add `servant-persistent` as another package in the `stack.yaml` so that we can use it:

```yaml
# in stack.yaml
packages:
- '.'
- location:
    git: git@github.com:parsonsmatt/servant-persistent
    commit: 98479a423609794ffa9b668b0ae13ae9a57be18e
  extra-dep: true
```

And we'll need to add `servant-persistent` as a dependency of our project:

```
-- in yo-dawg.cabal
build-depends:
  -- .......
  , servant-persistent
```

That should be all the setup we need to do in order to start using the stuff.

# Who controls the database?

Yesod comes with a bunch of stuff for models.
But `servant-persistent` already has database stuff and models defined.
How should we handle this?

At the day job, I factored the models out into their own package.
That's an option that has worked well, though it's a little more labor intensive.

You could allow the API and the website to have separate models, though that seems like a lot of duplication and shared concerns.

What we'll do, for simplicity, is rely on the models present in the `servant-persistent` app and delete the model code out of the Yesod repository.
In order to get this running, we'll need to delete the stuff relating to `Comment`s, as they're not present in the `servant-persistent` models.
We'll also need to delete the authentication code and `userIdent` references.

Thankfully, GitHub makes these changes easy to see. [Here's a commit link](https://github.com/parsonsmatt/yo-dawg/commit/03ecf35fcc7322f4aeddc1b145195bcfe791c6a7) that shows the changes to the base template necessary.

# Adding the Route

Our next task is to put the API somewhere.
`api` is a sensible place to put it, so let's add that to the routes file.

```
-- in config/routes

/api ServantPersistentR WaiSubsite getServantPersistent
{-
 [1]        [2]            [3]            [4]
 -}
```

The line has four components:

1. The route that we'll mount the API on
2. The data constructor to generate to route things *to* the API
3. The foundation of the subsite
4. The name of the function that will actually return the subsite.

When this code is added, we'll get an error in `Foundation.hs`, since it'll be trying to refer to `getServantPersistent`, which isn't defined.
The expected type of `getServantPersistent` is going to be a function that takes our `App` type in the `Foundation` and returns a `WaiSubsite`.

So we'll add the bare minimum to shut GHC up:

```haskell
-- in Foundation.hs

getServantPersistent :: App -> WaiSubsite
getServantPersistent = error "later"
```

Now, compilation succeeds.
The next step is to use the `WaiSubsite` constructor, which has the type `Application -> WaiSubsite`, where `Application` is a WAI application.

```haskell
-- in Foundation.hs

getServantPersistent :: App -> WaiSubsite
getServantPersistent = WaiSubsite . error "later"
```

Now, we're left with the question: Given our `App`, how do we get the `servant-persistent` `Application` out of it?

# Initializing the API

`servant-persistent` has a `Config` type that is in many ways similar to the `App` type in Yesod.
It contains all of the Stuff you need in order to get the API up and running, including the settings, database pool, etc.
Fortunately, the package also exposes a function `app :: Config -> Application`.
So all we need to do is get our hands on the `Config` data type, call that function, and we're set.

Fortunately, in this case, the `Config` is pretty simple: just an `Environment` and a `ConnectionPool`.
Yesod prefers to handle `Environment` by different (and better) means, so we'll just pass `Production` in.
The `ConnectionPool` is created in `Application.hs` function `makeFoundation`. 
Since we don't want to re-make the API Application on every request, we'll just go ahead and add the `Application` to the Yesod `App` data type.

In `Foundation.hs`, we'll make the following changes:

```haskell
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    -- vvv New! :D vvv
    , appSubApi      :: Application
    -- ^^^ New! :D ^^^
    }

getServantPersistent :: App -> WaiSubsite
getServantPersistent = WaiSubsite . appSubApi
```

And in `Application.hs`, we'll need to initialize the API when we get the connection pool.
Yesod does a bit of a hack by default here, so we'll respond in kind with a hack.
In the function `makeFoundation`, we'll modify the `mkFoundation` function defined in the `let` like so:

```haskell
-- Application.hs
import qualified Api as ServantPersistent
import Config (Config(..), Environment(Production))

-- down to makeFoundation ...
    let mkFoundation appConnPool = 
            let apiCfg = Config appConnPool Production
                appSubApi = ServantPersistent.app apiCfg
             in App {..}
-- ...
```

This ties everything together, and you'll be serving your Servant API out of a Yesod subsite.
Neat!
