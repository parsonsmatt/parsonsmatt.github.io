---
title: "Global IORef in Template Haskell"
date: 2021-04-21
layout: post
categories: programming
---

I'm investigating [a way to speed up `persistent` as well as make it more powerful](https://github.com/yesodweb/persistent/issues/1241#issuecomment-824128224), and one of the potential solutions involves persisting some global state across module boundaries.
I decided to investigate whether the "Global IORef Trick" would work for this.
Unfortunately, it doesn't.

On reflection, it seems obvious: the interpreter for Template Haskell is a GHCi-like process that is loaded for each module.
Loading an interpreter for each module is part of why Template Haskell imposes a compile-time penalty - in my measurements, it's something like ~100ms.
Not huge, but noticeable on large projects.
(I still generally find that `DeriveGeneric` and the related `Generic` code to be slower, but it's a complex issue).

Anyway, let's review the trick and obseve the behavior.

# Global IORef Trick

This trick allows you to have an `IORef` (or `MVar`) that serves as a global reference.
You almost certainly *do not* need to do this, but it can be a convenient way to hide  state and make your program deeply mysterious.

Here's the trick:

```haskell
module Lib where

import Data.IORef
import System.IO.Unsafe

globalRef :: IORef [String]
globalRef = unsafePerformIO $ newIORef []
{-# NOINLINE globalRef #-}
```

There are two important things to note:

1. You *must* give a concrete type to this.
2. You *must* write the `{-# NOINLINE globalRef #-}` pragma.

Let's say we give `globalRef` a more general type: 

```haskell
globalRef :: IORef [a]
```

This means that we woudl be allowed to write and read whatever we want from this reference.
That's bad!
We could do something like `writeIORef globalRef [1,2,3]`, and then `readIORef globalRef :: IO [String]`.
Boom, your program explodes.

Unless you *want* a dynamically typed reference for some reason - and even then, you'd better use `Dynamic`.

If you omit the `NOINLINE` pragma, then you'll just get a fresh reference each time you use it.
GHC will see that any reference to `globalRef` can be inlined to `unsafePerformIO (newIORef [])`, and it'll happily perform that optimization.
But that means you won't be sharing state through the reference.

This is a bad idea, don't use it.
I hesitate to even explain it.

# Testing the Trick

But, well, sometimes you try things out to see if they work.
In this case, they don't, so it's useful to document that.

We're going to write a function `trackString` that remembers the strings that are passed previously, and defines a value that returns those.

```haskell
trackString "hello"
-- hello = []

trackString "goodbye"
-- goodbye = ["hello"]

trackString "what"
-- what = ["goodbye", "hello"]
```

Here's our full module:

```haskell
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module Lib where

import Data.IORef
import System.IO.Unsafe
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

globalRef :: IORef [String]
globalRef = unsafePerformIO $ newIORef []
{-# NOINLINE globalRef #-}

trackStrings :: String -> Q [Dec]
trackStrings input = do
    strs <- runIO $ readIORef globalRef
    _ <- runIO $ atomicModifyIORef globalRef (\i -> (input : i, ()))
    ty <- [t| [String] |]
    pure
        [ SigD (mkName input) ty
        , ValD (VarP (mkName input)) (NormalB (ListE $ map (LitE . stringL) $ strs)) []
        ]
```

This works in a single module just fine.

```haskell
{-# language TemplateHaskell #-}

module Test where

import Lib

trackStrings "what"
trackStrings "good"
trackStrings "nothx"

test :: IO ()
test = do
    print what
    print good
    print nothx
```

If we evaluate `test`, we get the following output:

```
[]
["what"]
["good","what"]
```

This is exactly what we want.

Unfortunately, this is only module-local state.
Given this `Main` module, we get some disappointing output:

```haskell

{-# language TemplateHaskell #-}

module Main where

import Lib

import Test

trackStrings "hello"

trackStrings "world"

trackStrings "goodbye"

main :: IO ()
main = do
    test
    print hello
    print world
    print goodbye
```

```
[]
["what"]
["good","what"]
[]
["hello"]
["world","hello"]
```

To solve my problem, `main` would have needed to output:

```
[]
["what"]
["good","what"]
["nothx","good","what"]
["hello","nothx","good","what"]
["world","hello","nothx","good","what"]
```

# Module-local state in Template Haskell

Fortunately, we don't even need to do anything awful like this.
The `Q` monad offers two methods, [`getQ`](https://www.stackage.org/haddock/lts-17.9/template-haskell-2.16.0.0/Language-Haskell-TH-Syntax.html#v:getQ) and [`putQ`](https://www.stackage.org/haddock/lts-17.9/template-haskell-2.16.0.0/Language-Haskell-TH-Syntax.html#v:putQ) that allow module-local state.

```haskell
-- | Get state from the Q monad. Note that the state is 
-- local to the Haskell module in which the Template 
-- Haskell expression is executed.
getQ :: Typeable a => Q (Maybe a)

-- | Replace the state in the Q monad. Note that the 
-- state is local to the Haskell module in which the 
-- Template Haskell expression is executed.
putQ :: Typeable a => a -> Q ()
```

These use a `Typeable` dictionary, so you can store many kinds of state - one for each type!
This is a neat way to avoid the "polymorphic reference" problem I described above.

# How to actually solve the problem?

If y'all dare me enough I might write a follow-up where I investigate using a [compact region](https://hackage.haskell.org/package/compact-0.2.0.0/docs/Data-Compact-Serialize.html) to persist state across modules, but I'm terrified of the potential complexity at play there.
I imagine it'd work fine for a single threaded compile, but there'd probably be contention on the file with parallel builds.
Hey, maybe I just need to spin up a redis server to manage the file locks...
Perhaps I can install `nix` at compile-time and call out to a `nix-shell` that installs Redis and runs the server.
