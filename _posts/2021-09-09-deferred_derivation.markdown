---
title: "Deferred Derivation"
date: 2021-09-09
layout: post
categories: programming
---

## justifiably lazy orphans

(alternative subtitle: "I used the `TemplateHaskell` to destroy the `TemplateHaskell`")

(EDIT: 2021-11-05 - Having actually tried this approach in production, I now have an experience report and a warning. Scroll to the bottom for the details!)

At the day job, we use the `aeson-typescript` library to generate TypeScript types from our Haskell types.
One problem is that the library uses `TemplateHaskell` to do this, which means we have `TemplateHaskell` in almost all of our datatype-defining modules.
Due to the `TemplateHaskell` recompilation avoidance bug (any module that uses `TemplateHaskell` must always be recompiled if any transitive dependency is changed), this means we spend a lot of time recompiling a lot of modules that don't need to change.
Freeing a module from `TemplateHaskell` speeds up our build tremendously - not because `TemplateHaskell` is slow (it's very fast) but because *compiling at all* is slow.
The best way to speed something up is to spend 0 time doing it - don't do it at all!

Anyway, I investigated switching the library to a `Generics` based approach, but it's complicated and looked tricky, so I decided not to pursue that.
The library defines a type class [`HasJSONOptions`](https://hackage.haskell.org/package/aeson-typescript-0.3.0.1/docs/Data-Aeson-TypeScript-TH.html#t:HasJSONOptions), which lets you re-use the same JSON options for both `aeson`'s JSON encoding classes *and* the `TypeScript` class.

I had recently done some work with [`discoverEntities`](https://hackage.haskell.org/package/persistent-2.13.1.2/docs/Database-Persist-TH.html#v:discoverEntities), a `TemplateHaskell` function which gathers the `PersistEntity` instances in scope and collects their `entityDef (Proxy :: Proxy a)`.
I started wondering - can I use this trick to defer the derivation and omit the `TemplateHaskell`?
In preparation, I wrote [`discover-instances`](https://hackage.haskell.org/package/discover-instances-0.1.0.0/docs/DiscoverInstances.html), which generalized the above pattern (and was a fun exercise in typed `TemplateHaskell` quotes).

Now, we might have had an instance like this:

```haskell
data X = X { ... }

$(deriveJSON defaultOptions ''X)
$(deriveTypeScript defaultOptions ''X)
```

We can excise the `TemplateHaskell` entirely by using `Generics`-based derivation for the JSON classes, and specify an instance for the `HasJSONOptions` class.

```haskell
data X = X { ... }
    deriving stock Generic

instance HasJSONOptions X where
    getJSONOptions _ =
        defaultOptions

instance ToJSON X where
    toJSON =
        genericToJSON (getJSONOptions (Proxy :: Proxy X))

instance FromJSON X where
    parseJSON =
        genericFromJSON (getJSONOptions (Proxy :: Proxy X))
```

But we don't yet have that `TypeScript` instance.

Let's look at the module that actually generates our `TypeScript` code.

```haskell
module MakeTypeScript where

import Model.X
import Model.Y
import Model.Z

writeTypeScript :: IO ()
writeTypeScript = do
    writeFile frontendTypes $ concat
        [ makeTypeScriptFor (Proxy :: Proxy X)
        , makeTypeScriptFor (Proxy :: Proxy Y)
        , makeTypeScriptFor (Proxy :: Proxy Z)
        ]
```

This is the only place those instances are ever used.
With the above change (eg deleting the `deriveTypeScript` stuff), there's no longer an instance present.
But I can fix that by deriving the instance in this file.

```haskell
module MakeTypeScript where

import Model.X
import Model.Y
import Model.Z

$(deriveTypeScript (getJSONOptions (Proxy :: Proxy X)) ''X)
$(deriveTypeScript (getJSONOptions (Proxy :: Proxy Y)) ''Y)
$(deriveTypeScript (getJSONOptions (Proxy :: Proxy Z)) ''Z)

writeTypeScript :: IO ()
writeTypeScript = do
    writeFile frontendTypes $ concat
        [ makeTypeScriptFor (Proxy :: Proxy X)
        , makeTypeScriptFor (Proxy :: Proxy Y)
        , makeTypeScriptFor (Proxy :: Proxy Z)
        ]
```

This is deeply unsatisfying to me.
We have three points of repetition:

1. `import`ing a type
2. `derive`ing an instance for it
3. Mentioning the type in `writeTypeScript`.

Let's improve this.

First, we want to use [`discoverInstances`](https://hackage.haskell.org/package/discover-instances-0.1.0.0/docs/DiscoverInstances.html#v:discoverInstances) to splice in all the `HasJSONOptions` instances that are visible.
Second, we'll iterate over each instance, and derive the code there.

```haskell
module MakeTypeScript where

import Model.X
import Model.Y
import Model.Z

$(do
    decs <-
        forInstances 
            $$(discoverInstances @HasJSONOptions)
            $ \proxy@(Proxy :: Proxy a) -> do
                deriveTypeScript (getJSONOptions proxy) (nameForType proxy)
    pure (concat decs)
 )

writeTypeScript :: IO ()
writeTypeScript = do
    writeFile frontendTypes $ concat
        [ makeTypeScriptFor (Proxy :: Proxy X)
        , makeTypeScriptFor (Proxy :: Proxy Y)
        , makeTypeScriptFor (Proxy :: Proxy Z)
        ]
```

[`forInstances`](https://hackage.haskell.org/package/discover-instances-0.1.0.0/docs/DiscoverInstances.html#v:forInstances) is a convenience function that lets you operate on the proxy with the constraint in scope.

This totally works.
We've derived all of the instances of `TypeScript`, and now we're using them quite happily.

Now we're down to *two* points of repetition - importing a type and writing it specifically in `makeTypeScriptFor`.
We can't get rid of imports, so let's look at the `writeFile` list.
This is pretty easy to get rid of using our `SomeDict TypeScript`.

```haskell
module MakeTypeScript where

import Model.X
import Model.Y
import Model.Z

$(do
    decs <-
        forInstances 
            $$(discoverInstances @HasJSONOptions)
            $ \proxy@(Proxy :: Proxy a) -> do
                deriveTypeScript (getJSONOptions proxy) (nameForType proxy)
    pure (concat decs)
 )

writeTypeScript :: IO ()
writeTypeScript = do
    writeFile frontendTypes $ concat $ 
        withInstances 
            $$(discoverInstances @TypeScript)
            $ \proxy -> makeTypeScriptFor proxy
```

And we're done!

## Wins:

1. No more `TemplateHaskell` requirement in a module that defines an API type!
2. No more boring repetition!
3. We got to use fancy types!

## Losses:

1. Orphan instances :(
2. There's still TemplateHaskell even if it's localized
3. `Generic`-based deriving is slower, so a clean build will be worse

# UPDATE: Experience Report

Alright, so I've put this technique through it's paces on the work codebase, and it works, but it has an unforeseen negative consequence.
Here's what happens:

1. Types are enrolled in the "signal class", which is effectively free for compilation time.
2. But then we introduce a new module, `TypeScript.Instances`, which imports all the datatypes we expose on the front-end.
3. This module runs `$$(discoverInstances)` and then `deriveTypeScript` on every single type
4. This takes a long time! Like ~7 seconds.
5. And, because it is *downstream of every type*, we have to recompile the module basically whenever *any* module is touched.

To put it in a textual meme format,

* (!!!) Trade offer (!!!)
    * You get:
        * Recompilation avoidance for most of your modules
    * I get:
        * Forced recompilation of a big module in the hot path of a successful recompile, every single time, forever

This is not a good trade, unfortunately.

There are two work-arounds that I've thought about:

1. Moving the TypeScript code generation and instance derivation to the `executable` component that actually runs.
2. Moving all the types out-of-band of the modules that are more likely to change

## Moving to a separate package component

This essentially *defers* the problem, by saying: "I don't care about compiling this for my normal workflow. Please just only compile this when I specifically ask for it."
This means that your `ghcid` flow (or HLS, or whatever) will stop at this boundary and not check for compilation past that.
If you're in the habit of trying to build as much of the package as possible with each compile-step, then this completely defeats that.

To do this, you'd move the `MakeTypeScript` module from the `src/` or `library` component into an `executable` component.

I'm not a fan of this approach, and won't be pursuing it.

## Moving types out-of-band

So, this is a bit tricky.
The way a *lot* of our application is designed, we have our web application handlers defined in a module along with their request/response types.

For example, we might have a route called `FooR`, and it'd be defined in a module much like this:

```haskell
module Handler.Foo where

import Import.Handler

getFooR :: Text -> Handler FooResponse
getFooR fooName = do
    foo <- getFoo fooName
    pure $ FooResponse {..}

postFooR :: Handler FooResponse
postFooR = do
    FooRequest {..} <- parseJSONBody
    doStuff
    pure $ FooResponse {..}
```

We might have 1-2 datatype definitions in a module, with `Handler` and business logic taking up 100-1000 lines of code.
Recompiling this code every time is a drag, especially since the instances themselves are pretty quick.

In `MakeTypeScript`, we import each of our `Handler.*` modules.

```haskell
module MakeTypeScript where

import Handler.Foo
import Handler.Bar
import Handler.User
import Handler.Organziation
import ..........
```

Due to the transitive nature of the recompilation problem, this means that any change to any types *or* business logic upon those types will trigger a recompilation, not just of the `Handler`, but also the `MakeTypeScript`.

We can restructure the `Handler` modules to avoid this problem:

```haskell
module Handler.Foo.Types where

    -- this import has a *lot* fewer dependences in the application
    import Import.Handler.Types

    data FooRequest = FooRequest ...
    
    $(deriveJSONAndTypeScript defaultOptions ''FooRequest)
    
    data FooResponse = FooResponse ...
    
    $(deriveJSONAndTypeScript defaultOptions ''FooResponse)

module Handler.Foo where

    -- this import has mostly business logic
    import Import.Handler
    import Handler.Foo.Types
    
    getFooR :: Text -> Handler FooResponse
    getFooR fooName = do
        foo <- getFoo fooName
        pure $ FooResponse {..}
    
    postFooR :: Handler FooResponse
    postFooR = do
        FooRequest {..} <- parseJSONBody
        doStuff
        pure $ FooResponse {..}

    -- etc for a few hundred lines
```

By doing this, we now only recompile `Handler.Foo` when it *actually* needs to change.
If we can sufficiently separate the business logic and type declarations, then we can also avoid recompiling `MakeTypeScript` so often -

```haskell
module MakeTypeScript where

import Handler.Foo.Types
import Handler.Bar.Types
import ...
```

If these `Handler.*.Types` modules only ever depend on rarely-changing datatypes, then this should alleviate the problem.

### However...

A problem with the `Deferred Derivation` approach is that it is extremely easy to *break* the fast compilation times you get from it.
Since any `TemplateHaskell` at all causes The Recompilation Problem, skipping only a few parts of it will mostly just make your compilation slower.

The strategy for moving types out-of-band is also effective at solving the original problem.
By defining the types and logic separately, you don't have to needlessly recompile the types all of the time.
And this has a cascading incremental effect - the more module splitups you do, the faster things get, immediately.

# Maybe this is a bad idea

At the very least, the results of performing the experiment at work are: "let's not use this."

I do still think there's a lot of value in "signal classes" and `discoverInstances` for those classes to perform neat metaprogramming.
Deferring derivation of type classes that are primarily used for a single thing may not be the ticket, though.
