---
title: "Stealing Impl from Rust"
date: 2021-07-29
layout: post
categories: programming
---

With the new `OverloadedRecordDot` language extension, we can use the `.` character to access stuff on records.

```haskell
{-# language OverloadedRecordDot #-}

data User = User { name :: String }

main :: IO ()
main = do
    let user = User { name = "Matt" }
    putStrLn user.name
```

This is syntax sugar for the following code:

```haskell
import GHC.Records

data User = User { name :: String }

instance HasField "name" User String where
    getField (User n) = 
        n

main :: IO ()
main = do
    let user = User { name = "Matt" }
    putStrLn (getField @"name" user)
```

As it happens, we can *add* fields to a record.

```haskell
{-# language OverloadedRecordDot #-}

data User = User { name :: String }

instance HasField "age" User Int where
    getField user = 
        32

main :: IO ()
main = do
    let user = User { name = "Matt" }
    print user.age
```

This works, though it's a bit boring.

It's much more useful to have, say, virtual fields.

```haskell
data User 
    = User
    { firstName :: String
    , lastName :: String
    }

instance HasField "name" User String where
    getField user = 
        unwords [user.firstName, user.lastName]
```

This gives us a "virtual field," which can allow us to refactor code that depends on the record field in neat ways!

# Methods

So, those types, they don't have to be ordinary values.
They can be *methods*.

Or, y'know, functions, whatever, it's all the same in Haskell.

```haskell
instance HasField "greet" User (String -> IO ()) where
    getField self message = do
        putStrLn $ concat [message, ", ", self.name, "!"]

main :: IO ()
main = do
    let user = User { firstName = "Matt", lastName = "Parsons" }
    user.greet "UhhhHHH Excuse me WTF"
```

This prints out `UhhhHHH Excuse me WTF, Matt Parsons!`.
Which is pretty cool.

# `impl`

Rust has a [keyword `impl`](https://doc.rust-lang.org/std/keyword.impl.html), which is used in two ways:

1. Adding methods to a type.
2. Adding a trait to a type.

The linked docs tell the whole story, just about.

I don't know about you but I want nicer syntax than all the `instance HasField` stuff.
I wrote a little library that *should* do this:

```haskell
data User = User { name :: String }

impl ''User [d|

    greet :: String -> IO ()
    greet message = do
        putStrLn $ concat [message, ", ", self.name]
|]
```

It's relatively straightforward.
In pseudocode, it's implemented like this:

```haskell
impl :: Name -> Q [Dec] -> Q [Dec]
impl tyName qds = do
    decs <- qds
    let
        namesTypesExprs :: [(String, Type, Exp)]
        namesTypesExprs =
            getTypesAndExprs decs

    instances <- for namesTypesExprs $ \(name, typ, exp) -> do
        [d|
            instance HasField $(name) $(tyName) $(typ) where
                getField self = $(exp)
        |]
    pure (concat instances)
```

Unfortunately, I ran into a bit of a [blocking issue](https://gitlab.haskell.org/ghc/ghc/-/issues/20185), namely that GHC does not support `OverloadedRecordDot` in `TemplateHaskell` `QuasiQuotes` yet.
While I can work around it, I'd rather not bother until `OverloadedRecordDot` is fully supported by GHC.

# The Dealbreaker

There's no polymorphism allowed.

Like, at all.

You can't write:

```haskell
instance (Show a) => HasField "myPrint" User (a -> IO ()) where
    getField self a = do
        putStrLn (show a)
```

This fails the functional dependencies.
You can't write methods generic in `MonadIO m => HasField User (String -> m ())` either.

The functional dependencies seem pretty reasonable:

```haskell
class HasField sym r a | sym r -> a where
    getField :: r -> a
```

This means that the types `sym` and `r` *uniquely determine* the `a` type - or, that if you know what `sym` and `r` are, then you *always* know *exactly* what `a` is.
Since users of our isntance are able to select things like `IO`, `ReaderT () IO`, and `StateT Int IO` for this, you *can't* uniquely determine the result type just based on the answer.

Seems like `ImpredicativeTypes` should work here, but they apparently don't.

```haskell
instance HasField "myPrint" User (forall a. Show a => a -> IO ()) where ...
```

This fails with a syntax error, due to the way `OverloadedRecordDot` affects GHC's parser.

> [Bugs for the bug god!!](https://gitlab.haskell.org/ghc/ghc/-/issues/20186)

```haskell
instance HasField "myPrint" User (forall a . Show a => a -> IO ()) where ...
```

This fails because it is an illegal polymorphic type.

# Illegal Polymorphism

But wait - this is an *impredicative* type.
`ImpredicativeTypes` was a deprecated language extension, but I recall hearing that we landed support for them with [Quick Look Impredicativity](https://github.com/ghc-proposals/ghc-proposals/pull/274).
And, in GHC 9, [we have a proper `ImpredicativeTypes` behavior](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/impredicative_types.html#impredicative-polymorphism)!
We *definitely* are paying a big cost for it - the [Simplify Subsumption](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.0#simplified-subsumption) proposal gives *no* practical benefit  to programmers *except* that it gives additional power to Quick Look Impredicativity.

Unfortunately, enabling `ImpredicativeTypes` doesn't make this work - GHC still deems the above an illegal polymorphic type.
It turns out, you can't [put an impredicative type in an instance at all](https://gitlab.haskell.org/ghc/ghc/-/issues/20188).

Oh well.

# BREAKING NEWS

Okay, so I posted this, and was immediately offered a Prime Tip by Sandy Maguire.
Apparently [Richard Eisenberg](https://www.youtube.com/watch?v=ZXtdd8e7CQQ) has published a video stating how to defeat this.
The answer is to *demand the constraint in the context*.

So we can write `myPrint` like this:

```haskell
instance 
    ( Show a 
    , HasField "myPrint" User (a -> IO ())
    )
  => 
    HasField "myPrint" User (a -> IO ()) 
  where
    getField self a = do
        putStrLn $ concat [self.name, " says: ", show a]
```

That let's us write code like this:

```haskell
go :: IO ()
go = do
    let user = User { name = "Matt" }
    user.myPrint 'a'
    user.myPrint 3
```

Which evaluates like this:

```
$> go
Matt says: 'a'
Matt says: 3
```

Nice!

Deal *un*breaker.
