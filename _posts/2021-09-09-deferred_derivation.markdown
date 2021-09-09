---
title: "Deferred Derivation"
date: 2021-09-09
layout: post
categories: programming
---

## justifiably lazy orphans

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

# Wins:

1. No more `TemplateHaskell` requirement in a module that defines an API type!
2. No more boring repetition!
3. We got to use fancy types!

# Losses:

1. Orphan instances :(
2. There's still TemplateHaskell even if it's localized
3. `Generic`-based deriving is slower, so a clean build will be worse
