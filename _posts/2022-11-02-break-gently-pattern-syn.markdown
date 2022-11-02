---
title: "Break Gently with Pattern Synonyms"
date: 2022-11-02
layout: post
categories: programming
---

This is a really brief post to call out a nice trick for providing users a nice migration message when you delete a constructor in a sum type.

# The Problem

You have a sum type, and you want to delete a redundant constructor to refactor things.

```haskell
data Foo 
    = Bar Int 
    | Baz Char
    | Quux Double
```

That `Quux` is double trouble.
But if we simply delete it, then users will get a `Constructor not found: Quux`.
This isn't super helpful.
They'll have to go find where `Quux` came from, what package defined it, and then go see if there's a Changelog.
If not, then they'll have to dig through the Git history to see what's going on.
This isn't a fun workflow.

But, let's say you *really need end users to migrate off `Quux`*.
So we're interested in giving a compile error that has more information than `Constructor not in scope`.

Here's what some calling code looks like:

```haskell
blah :: Foo -> Int
blah x = case x of
    Bar i -> i
    Baz c -> fromEnum c
    Quux a -> 3
```

will give the output:

```haskell
/home/matt/patsyn.hs:24:5: error:
    Not in scope: data constructor ‘Quux’
   |
24 |     Quux a -> 3
   |     ^^^^
Failed, no modules loaded.
```

Fortunately, we can make this nicer.

GHC gives us a neat trick called [`PatternSynonyms`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html).
They create constructor-like things that we can match on and construct with, but that are a bit smarter.

## Matching

Let's redefine `Quux` as a pattern synonym on `Foo`.
We'll also export it as part of the datatype definition.

```haskell
{-# language PatternSynonyms, ViewPatterns #-}

module Wow (Foo (.., Quux)) where

data Foo
    = Bar Int
    | Baz Char

pattern Quux :: a -> Foo
pattern Quux i <- (const Nothing -> Just i)
```

This does something tricky: we always throw away the input with the `ViewPattern`, and we can summon whatever we want in the left hand side.
This allows us to provide whatever `a` is needed to satisfy the type.
This match will *never* succeed - so `Quux` behavior will never happen.

Now, we get a warning for the match:

```
[1 of 1] Compiling Main             ( /home/matt/patsyn.hs, interpreted )

/home/matt/patsyn.hs:25:5: warning: [-Woverlapping-patterns]
    Pattern match is redundant
    In a case alternative: Quux a -> ...
   |
25 |     Quux a -> 3
   |     ^^^^^^^^^^^
Ok, one module loaded.
```

But an error for constructing:

```
[1 of 1] Compiling Main             ( /home/matt/patsyn.hs, interpreted )

/home/matt/patsyn.hs:28:10: error:
    • non-bidirectional pattern synonym ‘Quux’ used in an expression
    • In the expression: Quux 3
      In an equation for ‘blargh’: blargh = Quux 3
   |
28 | blargh = Quux 3
   |          ^^^^
Failed, no modules loaded.
```

So we need to construct with it, too.
We can modify the pattern synonym by providing a `where`, and specifying how to construct with it.
Since we're intending to prevent folks from using it, we'll just use `undefined`.

```haskell
pattern Quux :: a -> Foo
pattern Quux i <- (const Nothing -> Just i) where
    Quux _ = undefined
```

With this, we get just the warning about a redundant pattern match.
Now it's time to step up our game by providing a message to the end user.

# Warnings

GHC gives us the ability to write `{-# WARNING Quux "migrate me pls" #-}`.
This can make sense if we expect that the runtime behavior of a program won't be changed by our pattern synonym.

So let's write a warning:

```haskell
pattern Quux :: a -> Foo
pattern Quux i <- (const Nothing -> Just i) where
    Quux _ = undefined

{-# WARNING 
  Quux 
    "Please migrate away from Quux in some cool manner. \
    \See X resource for migration tips." 
  #-}

```

Now, when compiling, we'll see the warnings:

```
/home/matt/patsynimp.hs:11:5: warning: [-Wdeprecations]
    In the use of data constructor ‘Quux’ (imported from PatSyn):
    "Please migrate away from Quux in some cool manner. See X resource for migration tips."
   |
11 |     Quux _ -> 3
   |     ^^^^

/home/matt/patsynimp.hs:11:5: warning: [-Woverlapping-patterns]
    Pattern match is redundant
    In a case alternative: Quux _ -> ...
   |
11 |     Quux _ -> 3
   |     ^^^^^^^^^^^

/home/matt/patsynimp.hs:14:10: warning: [-Wdeprecations]
    In the use of data constructor ‘Quux’ (imported from PatSyn):
    "Please migrate away from Quux in some cool manner. See X resource for migration tips."
   |
14 | blargh = Quux (3 :: Int)
   |          ^^^^

```

But this may not be good enough.
We may want to give them an error, so they can't build.

# `TypeError`

[`base` defines a type `TypeError`](https://www.stackage.org/haddock/lts-19.31/base-4.15.1.0/GHC-TypeLits.html#t:TypeError), which GHC treats specially - it raises a type error.
This isn't generally useful, but can be great for marking branches of a `type family` or type class `instance` as "impossible."
The error message can be fantastic for guiding folks towards writing correct code.

`PatternSynonym`s can have two sets of constraints: the first is *required* when constructing, and the second is *provided* when matching.
So let's just put an error in the first and see what happens:

```haskell
pattern Quux
    :: (TypeError ('Text "please migrate ..."))
    => ()
    => a -> Foo
pattern Quux i <- (const Nothing -> Just i) where
    Quux _ = undefined
```

Unfortunately, GHC blows up immediately while compiling the synonym!

```
[1 of 2] Compiling PatSyn           ( PatSyn.hs, interpreted )

PatSyn.hs:20:1: error: please migrate ...
   |
20 | pattern Quux
   | ^^^^^^^^^^^^...
Failed, no modules loaded.
```

We can't even `-fdefer-type-errors` this one. Are we hosed?

What about the second position?
Same problem. We can't put a bare `TypeError` in there at all.

Fortunately, we can have a lil' bit of laziness by introducing it as a *constraint*.

```haskell
class DeferredError
instance (TypeError ('Text "please migrate ...")) => DeferredError

pattern Quux
    :: DeferredError
    => DeferredError
    => a -> Foo
pattern Quux i <- (const Nothing -> Just i) where
    Quux _ = undefined
```

This actually *does* give us a warning now - at the `const Nothing -> Just i` line, we have a deferred type error.

This gives us the error behavior we want!

```
/home/matt/patsynimp.hs:14:10: error:
    • please migrate ...
    • In the expression: Quux (3 :: Int)
      In an equation for ‘blargh’: blargh = Quux (3 :: Int)
   |
14 | blargh = Quux (3 :: Int)
   |          ^^^^^^^^^^^^^^^
Failed, one module loaded.
```

We only get the one error - but if we delete it, we can see the other error:

```
[2 of 2] Compiling Main             ( /home/matt/patsynimp.hs, interpreted )

/home/matt/patsynimp.hs:11:5: error:
    • please migrate ...
    • In the pattern: Quux _
      In a case alternative: Quux _ -> 3
      In the expression:
        case x of
          Bar i -> i
          Baz c -> fromEnum c
          Quux _ -> 3
   |
11 |     Quux _ -> 3
   |     ^^^^^^
Failed, one module loaded.
```

What's fun is that we can actually provide *two* different messages.
Constructing something will give both error messages, and pattern matching only uses the "required" constraint.

This should make it *much* easier for end users to migrate to new versions of your library.

# Final Code and Errors

```haskell
{-# language PatternSynonyms #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ViewPatterns #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language DataKinds #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}

module PatSyn where

import Prelude
import GHC.Exts
import GHC.TypeLits

data Foo
    = Bar Int
    | Baz Char

class DeferredError (a :: ErrorMessage)
instance (TypeError a) => DeferredError a

pattern Quux
    :: DeferredError ('Text "please migrate (required constraint)")
    => DeferredError ('Text "please migrate (provided constraint)")
    => a -> Foo
pattern Quux i <- (const Nothing -> Just i) where
    Quux _ = undefined
```

Matching a constructor:

```
[2 of 2] Compiling Main             ( /home/matt/patsynimp.hs, interpreted )

/home/matt/patsynimp.hs:11:5: error:
    • please migrate (required constraint)
    • In the pattern: Quux _
      In a case alternative: Quux _ -> 3
      In the expression:
        case x of
          Bar i -> i
          Baz c -> fromEnum c
          Quux _ -> 3
   |
11 |     Quux _ -> 3
   |     ^^^^^^
Failed, one module loaded.

```

Using a constructor:

```
[2 of 2] Compiling Main             ( /home/matt/patsynimp.hs, interpreted )

/home/matt/patsynimp.hs:14:10: error:
    • please migrate (required constraint)
    • In the expression: Quux (3 :: Int)
      In an equation for ‘blargh’: blargh = Quux (3 :: Int)
   |
14 | blargh = Quux (3 :: Int)
   |          ^^^^^^^^^^^^^^^

/home/matt/patsynimp.hs:14:10: error:
    • please migrate (provided constraint)
    • In the expression: Quux (3 :: Int)
      In an equation for ‘blargh’: blargh = Quux (3 :: Int)
   |
14 | blargh = Quux (3 :: Int)
   |          ^^^^^^^^^^^^^^^
Failed, one module loaded.
```
