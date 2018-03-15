---
title: "Servant Route Smooshing"
date: 2018-03-14
layout: post
categories: programming
---

Haskell's `servant` library needs only a modest introduction -- it's an attempt to stuff the description of an API into the type system.
Using compile-time type-level programming, we're able to get a number of benefits:

- The server implements the type faithfully
- You can derive clients automagically
- You can get Swagger specification automagically
- You can get lots of testing facilities for free

This is all really neat!

However, because Haskell's [type system isn't as pleasant as the value-system](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html), this can get gnarly.
Servant has a very happy path -- but that path is very narrow.
Let's look at an API type that roughly describes a game:

```haskell
type Api
    = "player" 
        :> Capture "playerId" Int 
        :> "x" 
        :> Get '[JSON] Int
    :<|> "player" 
        :> Capture "playerId" Int 
        :> "y" 
        :> Get '[JSON] Int
```

This `Api` type describes two routes: either `player/:playerId/x` or `player/:playerId/y`, returning the coordinates of the given player.
All by itself, it doesn't *do* a whole lot.
However, we can write a `Server` and a `Client` for it:

```haskell
apiServer :: Server Api
apiServer = serveX :<|> serveY
  where 
    serveX :: Int -> Handler Int
    serveX playerId = return 42

    serveY :: Int -> Handler Int
    serveY playerId = return 24

getX :: Int -> ClientM Int
getY :: Int -> ClientM Int
getX :<|> getY = client (Proxy :: Proxy Api)
```

Now this -- this is really cool! 
Our client is free, and our server is type-checked.

# The DRYing

Now, the anti-repetition DRY part of your brain is going to see that route and want to factor parts of it out.
After all, the `"player" :> Capture "playerId" Int` part is repeated.
Servant has no problem with factoring it out:

```haskell
type Api'
    = "player" 
    :> Capture "playerId" Int
    :> (     "y" :> Get '[JSON] Int
        :<|> "x" :> Get '[JSON] Int
       )
```

The repetition is gone! Very cool.

Unfortunately... this complicates the types of the `Server` and `Client`.
Let's reuse the old implementation for the server and see what happens:

```haskell
apiServer' :: Server Api'
apiServer' = serveX :<|> serveY
  where
    serveX playerId = return 42
    serveY playerId = return 42
```

We get an error, reproduced below:

```
/home/matt/Projects/servant-smoosh/src/Lib.hs:41:14: error:
    • Couldn't match type ‘(p0 -> m0 Integer) :<|> (p1 -> m1 Integer)’
                     with ‘Int -> Handler Int :<|> Handler Int’
      Expected type: Server Api'
        Actual type: (p0 -> m0 Integer) :<|> (p1 -> m1 Integer)
    • In the expression: serveX :<|> serveY
      In an equation for ‘apiServer'’:
          apiServer'
            = serveX :<|> serveY
            where
                serveX playerId = return 42
                serveY playerId = return 42
   |
41 | apiServer' = serveX :<|> serveY
   |              ^^^^^^^^^^^^^^^^^^
```

What's going on here?

# `:<|>` does not distribute

The two API types we provide above end up describing the exact same API structure.
However, the *structure* of the type is different, and the operation does not in fact distribute.
What's it mean for something to distribute?

Let's look at addition and multiplication, a very simple form of distribution.
If we have $(x \times y) + (x \times z)$, we can factor out the multiplication of $x$.
That gives us $x \times (y + z)$, an expression that is exactly equal.

Ideally, we could factor out parameters in our servant API types, and they would "distribute" those parameters to all subroutes.

So, let's fix that initial type error, and we'll dig into some simplified implementation details of Servant to figure out why.
Instead of having a `Server` that contains two handlers, each a function from the captured `Int` to the return type, we have a function from an `Int` to a server of two handlers.
We can inspect this in GHCi with the `:kind!` command:

```haskell
>>> :kind! Server Api
Server Api :: *
= (Int -> Handler Int) :<|> (Int -> Handler Int)
>>> :kind! Server Api'
Server Api' :: *
= Int -> Handler Int :<|> Handler Int
```

`:kind!` takes a type and tries to normalize it fully -- this means applying all available type synonyms and computing all type families.

We have factored out the parameter, and this has complicated our server type.
Here's the new server for it:

```haskell
apiServer' :: Server Api'
apiServer' playerId = serveX :<|> serveY
  where
    serveX = return 42
    serveY = return 42
```

`apiServer'` accepts the `playerId` parameter from the capture.
`serveX` and `serveY` are mere `Handler Int`s, now.
They have access to `playerId` because it's in scope, but if you factor those functions out into top-level definitions, you'd need to pass it explicitly.

# What about the client?

We can use `:kind!` with the `Client` type as well:

```haskell
>>> :kind! Client Api
Client Api :: *
= (Int -> ClientM Int) :<|> (Int -> ClientM Int)
>>> :kind Client Api'
Client Api' :: *
```

Huh, that's -- weird.
`Client Api'` has the kind `*` -- it's an ordinary value.

Let's instead look at the type of the derived client, using the `client` function:

```haskell
>>> :t client (Proxy :: Proxy Api)
client (Proxy :: Proxy Api)
  :: (Int -> ClientM Int) :<|> (Int -> ClientM Int)
>>> :t client (Proxy :: Proxy Api')
client (Proxy :: Proxy Api') 
  :: Int -> ClientM Int :<|> ClientM Int
```

Ah! So `client` with the `Api'` type does not give us a pair of client functions, but rather, a function that returns a pair of clients.
This makes derivation much less pleasant.

```haskell
mkClient :: Int -> ClientM Int :<|> ClientM Int
mkClient = client (Proxy :: Proxy Api')

getX :: Int -> ClientM Int
getX i = getX'
  where
    (getX' :<|> _) = mkClient i

getY :: Int -> ClientM Int
getY i = getY'
  where
    (_ :<|> getY') = mkClient i
```

This gets *dramatically* worse as the number of parameters goes up, and as the level of nesting increases.

At this point, I strongly recommend keeping your API types as flat and repetitive as possible. 
Doing otherwise takes you off Servant's happy path.

# How does this happen?

Servant uses a technique that I refer to as "inductive type class programming."
It provides a lot of extensibility, and is super cool.

Let's reproduce a bit of Servant's stuff:

```haskell
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Butler where

import GHC.TypeLits

data thing :> route
infixr 9 :>

data alt1 :<|> alt2 = alt1 :<|> alt2
infixr 8 :<|>

data Capture (sym :: Symbol) typ

data Get a
```

This is all we need to define our own type-level APIs!
We have a type operators `:>` and a data type `:<|>`, and a `Capture` type that describes a named parameter.

Here's our simplified API using the Butler types:

```haskell
type BPI
    = "player"
        :> Capture "playerId" Int
        :> "x"
        :> Get Int
    :<|> "player"
        :> Capture "playerId" Int
        :> "y"
        :> Get Int
```

Now, we want to implement a `Server` for it. 
But first, we need a way of describing what a `Server` for a given API type looks like.
We will ignore actually serving the API using a `Server`, and instead focus on defining the handlers.
For our purposes, the `Server` class will be quite simple:

```haskell
class HasServer x where
    type Server x
```

Now the fun begins.
We are going to write a lot of overlapping and orphan instances.
That's just part of the deal with this style of programming.
We're going to start with our base case: the `Get` handler.

```haskell
instance HasServer (Get a) where
    type Server (Get a) = IO a
```

The handler for a simple `Get a` is an `IO a`.

What about alternation?
If we have `left :<|> right`, then it makes sense that we'd need for `left` to be serve-able and `right` to be serve-able.
We express this by requiring `HasServer` instances in the instance context.

```haskell
instance 
    ( HasServer left
    , HasServer right
    )
    => HasServer (left :<|> right) where
    type Server (left :<|> right) =
        Server left :<|> Server right
```

So the server for an alternation of APIs is the alternation of the servers of those APIs.

Let's do `Capture` now -- that one is a bit interesting!
In order to handle the Capture, we need to take the capture-d thing as a parameter.

```haskell
instance HasServer (Capture paramName paramType) where
    type Server (Capture paramName paramType) =
        -- ...?
```

Well, that doesn't quite work out.
We don't have the rest of the server to delegate to.
That's because we use `:>` for chaining combinators.
We'll need to write the instance for `Capture` using the `:>` combinator to make it flow.

```haskell
instance
    ( HasServer rest
    )
    => HasServer (Capture paramName paramType :> rest) where
    type Server (Capture paramName paramType :> rest) =
        paramType -> Server rest
```

We need to enable `FlexibleInstances` for this one.
Let's try it out so far:

```haskell
>>> :kind! Server (Capture "hey" Int :> Get String)
Server (Capture "hey" Int :> Get String) :: *
= Int -> IO [Char]

>>> :kind! Server (Capture "hey" Int :> Get String :<|> Get Char)
Server (Capture "hey" Int :> Get String :<|> Get Char) :: *
= (Int -> IO [Char]) :<|> IO Char
```

Nice! This works out.
Let's add the instance for `"hey" :> rest` symbols:

```haskell
instance
    ( HasServer rest
    )
    => HasServer (skipMe :> rest) where
    type Server (skipMe :> rest) =
        Server rest 
```

Because we're only dealing with handler types, we just ignore this.
Unfortunately, GHC isn't happy with this:

```haskell
    Conflicting family instance declarations:
      forall k1 k2 (skipMe :: k2) (rest :: k1).
        Server (skipMe :> rest) = Server rest
          -- Defined at /home/matt/Projects/servant-smoosh/src/Butler.hs:51:10
      forall k (paramName :: Symbol) paramType (rest :: k).
        Server (Capture paramName paramType :> rest) = paramType
                                                       -> Server rest
          -- Defined at /home/matt/Projects/servant-smoosh/src/Butler.hs:58:10
   |
51 |     type Server (skipMe :> rest) =
   |          ^^^^^^^^^^^^^^^^^^^^^^^^^...
```

Frankly, this one mystified me.
Google didn't help me find it.
This kind of programming puts you into a fairly hostile territory, and it becomes difficult to figure out how to solve problems.
It requires a lot of experimentation, guesswork, and luck.

The fix was weird: provide a kind annotation to `skipMe`:

```haskell
instance
    ( HasServer rest
    )
    => HasServer ((skipMe :: Symbol) :> rest) where
    type Server (skipMe :> rest) =
        Server rest
```

And now we're back in business!
We can write out the handler type for our server:

```haskell
type BPI
    = "player"
        :> Capture "playerId" Int
        :> "x"
        :> Get Int
    :<|> "player"
        :> Capture "playerId" Int
        :> "y"
        :> Get Int

bpiServer :: Server BPI
bpiServer = handleX :<|> handleY
  where
    handleX :: Int -> IO Int
    handleX _ = return 32
    handleY :: Int -> IO Int
    handleY _ = return 35
```

# What about the client?

Writing out the client is actually very similar to the server.
We create a type class `HasClient`, and write instances for all the various parts of the chain.
Since we're not actually serving or requesting anything here, I'll omit that part.
The small server pretend implementation is sufficient for us to continue.

# Smooshing the Servant

But... we *want* to have a nice DRY API type with nesting!
That eliminates a lot of boilerplate and makes writing handlers and clients quite nice.
Therefore, we need some way of distributing the parameters.

Type level programming in Haskell is quite hairy.
It's generally easier to sketch out a value-level program, desugar it, and then port it to the type level than it is to implement it directly.

We'll start by implementing a highly simplified version of the routes, at the value level:

```haskell
module Smoosh where

data Route
    = Route :> Route
    | Route :<|> Route
    | Capture String 
    | Path String
    | Get Int

infixr 9 :>
infixr 8 :<|>
```

This more-or-less mirrors the shape of the routes in Servant.
Our goal is to take a value that nests parameters, like this:

```haskell
nested :: Route
nested =
    Path "player"
    :> Capture "playerId"
    :> (Get 3 :<|> Get 4)
```

and flatten it out into a route that looks like this:

```haskell
flat :: Route
flat =
    Path "player"
        :> Capture "playerId"
        :> Get 3
    :<|> Path "player"
        :> Capture "playerId"
        :> Get 4
```

We'll walk up the route tree, collecting the Captures into a list, and when we hit a `:<|>` branch, we distribute the captures to both sides of the alternation.

```haskell
smooshRoute :: Route -> Route
smooshRoute = go []
  where
    go captures r =
        case r of
            Capture str :> rest ->
                go (Capture str : captures) rest
            Path str :> rest ->
                go (Path str : captures) rest
            Get i ->
                applyCaptures captures (Get i)
            r1 :<|> r2 ->
                applyCaptures captures (smooshRoute r1)
                :<|> applyCaptures captures (smooshRoute r2)

applyCaptures :: [Route] -> Route -> Route
applyCaptures [] route =
    route
applyCaptures (x:xs) route =
    applyCaptures xs (x :> route)

test :: Bool
test = smooshRoute nested == flat
```

You might note that `applyCaptures` could be rewritten using `foldl'`.
We won't do that, because you don't have `foldl'` at the type level.

# Port to the Types

Now that we've implemented this at the value level, it's relatively straight forward to desugar it and bring it to the type level, once you know the desugaring rules.

- There are no case expressions, so all pattern matching must be done at the top level.
- There are no `where` blocks, so all expressions must be at the top level.

This desugaring gives us this implementation:

```haskell
smooshRoute' :: Route -> Route
smooshRoute' = smooshHelper []

smooshHelper :: [Route] -> Route -> Route
smooshHelper captures (Capture str :> rest) =
    smooshHelper (Capture str : captures) rest
smooshHelper captures (Path str :> rest) =
    smooshHelper (Path str : captures) rest
smooshHelper captures (Get i) =
    applyCaptures captures (Get i)
smooshHelper captures (r1 :<|> r2) =
    applyCaptures captures (smooshRoute' r1)
    :<|> applyCaptures captures (smooshRoute' r2)
```

Now, we've got to make a choice: how do we do this at the type level?
Type classes, or type families?
Let's try type families first.

We'll start with the simplified `Butler` types we defined earlier, and then port to the more complicated `servant` types.
The translation works out relatively simply:

```haskell
type SmooshRoute route = Smoosh '[] route

type family Smoosh xs rt where
    Smoosh xs (Capture pname pty :> rest) =
        Smoosh (Capture pname pty ': xs) rest
    Smoosh xs ((sym :: Symbol) :> rest) =
        Smoosh (sym ': xs) rest
    Smoosh _ (Get i) =
        Get i
    Smoosh xs (r1 :<|> r2) =
        ApplyCaptures xs (SmooshRoute r1)
        :<|> ApplyCaptures xs (SmooshRoute r2)

type family ApplyCaptures xs r where
    ApplyCaptures '[] r =
        r
    ApplyCaptures (x ': xs) r =
        ApplyCaptures xs (x :> r)
```

Unfortunately, this doesn't work :(
Haskell's type level lists require that every type inside has the same kind, and this complains about `Symbol` and `*` not aligning.
We'll write a wrapper for `Symbol` and then special case in unpacking:

```haskell
data SWrap s

type family Smoosh xs rt where
    Smoosh xs (Capture pname pty :> rest) =
        Smoosh (Capture pname pty ': xs) rest
    Smoosh xs ((sym :: Symbol) :> rest) =
        Smoosh (SWrap sym ': xs) rest
    Smoosh xs (Get i) =
        ApplyCaptures xs (Get i)
    Smoosh xs (r1 :<|> r2) =
        ApplyCaptures xs (SmooshRoute r1)
        :<|> ApplyCaptures xs (SmooshRoute r2)

type family ApplyCaptures xs r where
    ApplyCaptures '[] r =
        r
    ApplyCaptures (SWrap x ': xs) r =
        ApplyCaptures xs (x :> r)
    ApplyCaptures (x ': xs) r =
        ApplyCaptures xs (x :> r)
```

Now, does it work?

```haskell
type BPI' = 
    "player" 
    :> Capture "playerId" Int
    :> (     "x" :> Get Int
        :<|> "y" :> Get Int
    )

bpiServer' :: Server (SmooshRoute BPI')
bpiServer' = handleX :<|> handleY
  where
    handleX :: Int -> IO Int
    handleX _ = return 32
    handleY :: Int -> IO Int
    handleY _ = return 35
```

GHC doesn't complain! We did it! Awesome!

# Port to Servant

Alright, it's time to level this thing up.
Let's port to `servant` and see if it works.

```haskell
type Distribute route = Flatten '[] route

data SWrap (s :: Symbol)

type family Flatten xs route where
    Flatten xs ((sym :: Symbol) :> rest) =
        Flatten (SWrap sym ': xs) rest
    Flatten xs ((x :: *) :> rest) =
        Flatten (x ': xs) rest
    Flatten xs (Verb meth code typs a) =
        ApplyCaptures xs (Verb meth code typs a)
    Flatten xs (r1 :<|> r2) =
        ApplyCaptures xs (Distribute r1)
        :<|> ApplyCaptures xs (Distribute r2)


type family ApplyCaptures xs r where
    ApplyCaptures '[] r =
        r
    ApplyCaptures (SWrap x ': xs) r =
        ApplyCaptures xs (x :> r)
    ApplyCaptures (x ': xs) r =
        ApplyCaptures xs (x :> r)
```

OK, this is the port.
I changed `Get` to `Verb` and added all the type parameters.
Everything else gets collected and distributed out to all the API leaves.

Let's write the server using this:


```haskell
distributed :: Server (Distribute Api')
distributed = serveX :<|> serveY
  where
    serveX _ = return 32
    serveY _ = return 42
```

But, it doesn't work.
We get this error:

```haskell
/home/matt/Projects/servant-smoosh/src/Lib.hs:54:15: error:
    • Couldn't match type ‘ServerT
                             (Flatten '[SWrap "y"] (Verb 'GET 200 '[JSON] Int)) Handler’
                     with ‘m0 a0’
      Expected type: Server (Distribute Api')
        Actual type: (Int -> m0 a0) :<|> (Int -> m1 a1)
      The type variables ‘m0’, ‘a0’ are ambiguous
    • In the expression: serveX :<|> serveY
      In an equation for ‘distributed’:
          distributed
            = serveX :<|> serveY
            where
                serveX _ = return 32
                serveY _ = return 42
   |
54 | distributed = serveX :<|> serveY
   |               ^^^^^^^^^^^^^^^^^^
```

It's got a type error.
If we look at that type error, we see that the `Flatten` type family is still there.

GHC does a thing where it gets "stuck" if a type family doesn't match anything.
Rather than saying "I can't figure this type family out, you must have made a mistake," it just carries the type on in the non-reduced state.
This is totally bizarre if you don't know what you're looking for.
So if you're type-level-hacking and you see a type family application, that means that it failed to match a case.

Specifically, it seems like we missed the `Flatten xs (Verb _ _ _ _)` case.
Why is that?
Let's inspect it:

```haskell
    Flatten xs (Verb meth code typs a) =
        ApplyCaptures xs (Verb meth code typs a)
```

Hmmm.. What extensions are enabled?
The behavior of type level programming in Haskell is dependent on the extensions we provide.

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
```

`TypeFamilies` enables, well, type families.
`UndecidableInstances` is needed for the recursion in `ApplyCaptures`.
`KindSignatures` allows us to write `data SWrap (s :: Symbol)`.
`DataKinds` lets us promote values-to-types and types-to-kinds.
And `TypeOperators` lets us use operators in types.

But you know what GHC does with type variables that don't have a kind signature?
By default, it infers that they're of kind `*`!
This is the explicit case that we've defined:

```haskell
Flatten 
    xs 
    (Verb (meth :: *) (code :: *) (typs :: *) (a :: *)) 
    = ApplyCaptures xs (Verb meth code typs a)
```

And the case we're trying to apply it to is this:

```haskell
Flatten '[SWrap "y"] (Verb 'GET 200 '[JSON] Int)
```

Ah! `'GET`, `'[JSON]` and `200` don't have kind `*`!
That is the trick.
So we make an addition to our extensions list:

```haskell
{-# LANGUAGE PolyKinds #-}
```

And now our implementation works!

Does the client work?


```haskell
getX' :: Int -> ClientM Int
getY' :: Int -> ClientM Int
getX' :<|> getY' = client (Proxy :: Proxy (Distribute Api'))
```

Yes, yes it does.

# Take aways

- Type level programming is hard and full of minefields.
- Type class inductive programming gives awful error messages
- Servant is fantastic, but nontrivial modifications and extensions require intense knowledge of GHC's weird flavor of type level programming.
