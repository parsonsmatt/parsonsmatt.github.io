---
title: "On Naming Things: Library Design"
date: 2017-06-23
layout: post
categories: programming
---

Perhaps you've heard this joke:

> There are only two hard problems in computer science: Naming things, cache invalidation, and off-by-one errors

Lol.

Naming things ends up being actually pretty difficult!
It's a nontrivial problem in library design, and there are interesting constraints imposed by the technologies we use.
There are a number of best practices and guidelines available for *using* libraries that make code easier to read and understand.
But we don't have compelling guidelines available for actually *writing* these libraries.

I've written a few libraries now and have tried out different naming and exporting conventions.
I've developed a bit of a feel for how it is to write and use them, and I'm going to put out my personal preferences and opinions on library design here.
This post will be discussing the Haskell programming language and ecosystem.

# Usage best practices

So you've got a bare Haskell module:

```haskell
module Main where

main :: IO ()
main = do
  ...
```

The `module $NAME where` starts the module definition, followed by a list of imports, and then declarations.
There are no imports declared.
So you know that all terms are coming from the `Prelude` module.
If you've enabled `NoImplicitPrelude` language extension, then you won't even have that in scope!

As we add imports, we add new terms.
Each new term might be unfamiliar to a programmer who is unfamiliar with the import.
As you add more and more imports, it's _import_ant (sorry) to make it easy for people to find where that term comes from.

Consider this import list:

```haskell
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Map.Strict                      as Map
import           Data.Swagger
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Language (Update)
import           Servant
```

If a new person comes upon a term that they don't understand, where did it come from?
It could be any of those imports.
There are typically two proposed solutions: explicit import lists, and qualified imports.

## Qualified Imports

```haskell
import qualified Data.Aeson      as Aeson
import qualified Data.Aeson.TH   as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Swagger    as Swagger
import qualified Database.Esqueleto as Esqueleto
import qualified Database.Esqueleto.Internal.Language (Update)
import qualified Servant
```

Unfortunately, this leads to extremely verbose code, and also makes operators super annoying to use.
Consider this example usage:

```haskell
type Api = "hello" Servant.:> Servant.Get '[Servant.JSON] Hello

data Hello = Hello { helloMap :: Map.Map String Int }

Aeson.deriveJSON Aeson.defaultJSONOptions ''Hello

-- or,
instance Aeson.ToJSON Hello where
    toJSON hello = Aeson.object
        [ "map" Aeson..= Aeson.toJSON (helloMap hello)
        ]
```

Gross!
This makes the code way noisier, and more verbose.
Everyone pays a significant cost when writing and reading this code.
Only new people to the codebase are benefitted, and even then, only for a short time.

It's common for the module name to be shortened, so `Data.ByteString` becomes `BS` or `B`, and `Data.HashMap.Strict` becomes `M`.
Sometimes you'll have `Data.Text`, `Data.Text.Encoding`, and `Data.Text.IO` all qualified under `T` or `Text`, which make it less easy to figure out where the term is coming from.

Qualified imports are great sometimes, but they don't seem to be a great solution *all* of the time.
So typically people use another strategy for making the namespace clean:

## Explicit Export Lists

You can also list out all of the terms that you use explicitly after the import.
This is a good practice, because it doesn't make the code more verbose, it just makes the import lists bigger.

Here's the previous code snippet and import list, but with explicit imports:

```haskell
import Servant    ((:>), Get, JSON)
import Map        (Map)
import Data.Aeson (ToJSON(..), object, (.=))

type Api = "hello" :> Get '[JSON] Hello

data Hello = Hello { helloMap :: Map String Int }

deriveJSON defaultJSONOptions ''Hello

-- or,
instance ToJSON Hello where
    toJSON hello = object
        [ "map" .= toJSON (helloMap hello)
        ]
```

This looks a lot cleaner.
Anyone that wants to know where a term comes from can simply search for it in the import lists.
However, it requires developer discipline or tooling to keep the import lists up to date.
If you update the API type to include the `Post` type, then you'll get a compile failure to update the import list.
This can be somewhat frustrating to work with.

There's another approach that some libraries use:

## Just stick me in my own module!

Some libraries, like Parsec, Esqueleto, etc. want to be used in their own module.
In my projects at work, I typically will have a module structure like:

```haskell
module App.SomeType where

-- type definition, functions for operating on the type, etc

module App.SomeType.Parser where

import Data.Attoparsec.ByteString

-- the parser definition
```

Why? The exports of `Attoparsec` and the encouraged style of writing functions have a tendency to collide with the names and functions for actually using the types.
These aren't typically imported or cared about: most clients of a `Parser` module just want `parseThing :: ByteString -> Either Error Thing`, not `thing :: Parser Thing` or `someSubComponentOfThing :: Parser Whatever`.

Likewise, the SQL library Esqueleto builds upon the Persistent database library for writing more advanced queries.
It redefines some of the terms and operators so that the language looks consistent with the Persistent DSL for queries and updates, but these are name collisions.
Additionally, the eDSL defines a bunch of common names like `on`, `from`, and `select`, which easily collide with other modules.
So I'll typically have a module structure like:

```haskell
module App.Models.SomeModel where

import Database.Persistent

-- functions specific to the model

module App.Query.SomeModel where

import Database.Esqueleto

-- queries specific to the model
```

where I can use Esqueleto's full edsl without having to worry about import/export business.

## Moderation

The problem of "where does this term come from" manifests mostly in very large modules with a ton of imports.
It's not an issue to find where a term is from if you have a 100 line module with 5 imports.
If you have a 1,000 line module with 20 imports, you're in trouble.
By breaking your modules up into smaller logical chunks, you can avoid this problem, at the expense of having your code spread out more.

Most codebases use a combination of qualified imports, explicit export lists, and open imports.
The decision tends to be made in terms of some combination of taste and the *design* of the library/module that you're importing.
Consider these modules:

```haskell
-- 1. Designed for qualified import
import           Data.Map (Map)
import qualified Data.Map as Map

-- 2. Basic enough that they don't need 
--    to be introduced
import           Control.Monad
import           Control.Applicative

-- 3. Designed for *unqualified* import
import           Data.IORef 
import           Control.Concurrent

-- 4. Obscure enough that you might want
--    to have an explicit import list
import           Control.Arrow (first, (&&&))
```

This is a somewhat moderate import strategy.
Unusual terms are imported explicitly, common terms are imported implicitly.
Some libraries are designed to be imported and used in a specific manner.

## What do we want?

Let's summarize what we want out of our import/export situation:

1. Easy to find where an identifier comes from
2. Not excessively verbose
3. Tooling isn't necessary to use the strategy
4. It's easy to write tooling for the strategy

With all that out of the way, I think I'm ready to talk about library design.

# Library Design

There are a few ways to design libraries to handle the import/export pain.

1. Qualified Import
2. Module Isolation
3. Open import

## Qualified Import

The `containers` library is *designed* to be imported qualified -- that is, you *must* import it qualified in order to use it.
If you don't, you get ambiguous term errors.
Any code that uses it typically has these two imports:

```haskell
import qualified Data.Map as Map
import           Data.Map (Map)
```

so that you can use the `Map` term unqualified and the functions for operating on `Map`s qualified, like so:

```haskell
someMap :: Map String Int
someMap = 
    Map.insert "hello" 3 Map.empty
        `Map.union` 
            Map.singleton "bar" 2
```

This has a problem: If you want to write a custom Prelude to cut down on import related boilerplate, you're unable to do so with this strategy.

```haskell
module MyPrelude
  ( module MyPrelude
  , module X
  ) where

import Data.Map (Map) as X
import Data.Set (Set) as X
import Data.Text (Text) as X
import Data.ByteString (ByteString) as X
```

When we import `MyPrelude`, we get the type names in scope.
This is an improvement.
But we still need to write out all the qualified imports to use the types:

```haskell
module Main where

import MyPrelude

import qualified Data.Map as Map
```

There is currently *no way* to do a qualified reexport, which would fix this issue.

If you are intending for your module to be used qualified, I'd recommend making the *intended* name available as a top level module.
Consider my library `monad-metrics`, which is designed for qualified import:

```haskell
import           Control.Monad.Metrics (MonadMetrics(..))
import qualified Control.Monad.Metrics as Metrics
```

This is a lot of typing! Instead, in a new version of the library, I will do:

```haskell
import           Control.Monad.Metrics 
import qualified Metrics
```

where `Control.Monad.Metrics` will export the types, and `Metrics` will export the functions for operating on them.
This cuts down on the effort required by the user.

This scheme requires a lot of maintenance on the part of the user, or a dependency on tooling that may or may not be available for a user's editor solution.
Furthermore, this encourages a style of naming where the same basic identifier gets used many times: [`empty`](http://hoogle.haskell.org/?hoogle=empty&scope=set%3Astackage) is used 25 times in the Stackage library.
This makes it more difficult for tooling to know what to suggest in these cases.

## Module Isolation

This strategy harkens back to the Esqueleto and Parsec examples I presented earlier.
It also applies to some other libraries I've used, like the Swagger library.
This is the easiest thing to do -- you stop caring about stepping on anyone's toes, and require that your users define functions that *use* your library in encapsulated, isolated modules, that they then reexport however they like.
This makes a lot of sense when you're defining an EDSL (embedded domain specific language) for working with something.
Parsers, SQL queries, HTML DSLs, and Swagger definitions all fall into this role.
Data structures and web servers typically don't.

This approach puts a tax on users: it requires that they break functionality into a separate module.
This is typically a good thing, but it asks more of users than a qualified import strategy or an open import strategy.
Typically, there won't be that many libraries that can use this scheme productively.

## Open Import

The open import strategy is quickly becoming my favorite.
The library is designed such that you can just import the whole thing without an import list and it's easy enough to find it.
If your library uses operators, you're strongly encouraging your users to use this strategy, even if the rest of your library doesn't fit it well.

To do this, you'll need to incorporate a bit of redundant information into the identifiers you use.
This is a sort of Hungarian notation.

Let's look at some examples of libraries that take this design.

### Data.IORef

```haskell
import Data.IORef
import Control.Concurrent

main = do
    ref <- newIORef 100
    fix $ \loop -> do
        val <- readIORef ref
        if val >= 0
            then do
                modifyIORef ref (subtract 1)
                loop
            else 
                putStrLn "Done!"
```

### Control.Concurrent.STM

```haskell
import Control.Concurrent
import Control.Concurrent.STM

main = do
    q <- newTQueue
    forkIO $ forever $ 
        print =<< atomically (readTQueue q)
    forM_ [1..1000] $ \i ->
        forkIO (atomically (writeTQueue q i))
```

### Lucid

```haskell
import Lucid

view :: Html ()
view = do
    h1_ "Hello world!"
    div_ $ do
        p_ $ do
            "The consistent naming scheme "
            "and specific identifier name "
            "choices make this an easy lib"
            "rary to use with an open impo"
            "rt."
        ul_ $ do
            li_ "Lucid is just great."
            li_ "Fave HTML lib fo sho"
```

Now, where does `newIORef` come from?
What does it result in?
It creates a new `IORef`.
Likewise, what about `newTQueue`?
It creates a new `TQueue`.

The Lucid functions all follow a simple convention that make them easy to use and purposefully avoid collision with other identifiers: the `_` suffix of an HTML tag makes it easy to know that an identifier is from `Lucid`.
The only collision I notice ordinarily is `for_`.

If these modules were designed with qualified import in mind, we'd write `TQueue.new`, `IORef.new`, or `Lucid.h1_` instead.
However, that would cause us a few problems:

1. You'd have to write `import qualified Data.IORef as IORef` and `import Data.IORef (IORef)` in all of your imports that use it
2. You can't reexport the functions from `IORef` anymore, so you need to explicitly import them every time, leading to more boilerplate in a custom prelude.

How would `Data.Map` look like with this scenario?

```haskell
import Data.Map

main = do
  let a = insertMap "hello" 0 emptyMap
      b = singletonMap "goodbye" 1
      c = unionMap a b
  print c
```

Aesthetically, I think I prefer `Map.insert` over `insertMap`.
Unfortunately, until we're able to re-export modules qualified, we're unable to use the qualified imports conveniently.

# IMO

In my opinion, desiging libraries for open import is the most convenient and useful way to go.
Alternatively, it *would* be nice to have a Template Haskell function that can take a module designed for qualified import and mechanically convert it to this style.
Then you could design with qualified import in mind and people that want an open import strategy could simply use the function and make their own.
