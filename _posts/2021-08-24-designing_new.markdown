---
title: "Designing New"
date: 2021-08-24
layout: post
categories: programming
---

I want a better way of constructing Haskell records.

Let's compare and contrast the existing ways.
We'll be using this datatype as an example:

```haskell
data Env = Env
    { accountId :: String
    , accountPassword :: String
    , requestHook :: Request -> IO Request
    , responseHook :: Response -> IO Response
    }
```

This type is an `Env` that you might see in a `ReaderT Env IO` integration with some external service.
We can attach request hooks and response hooks.

# Function Arguments

The simplest and *most* boring way is to pass function arguments.

```haskell
env :: Env
env = Env "asdfadf" "hunter42" pure pure
```

This is undesirable for a few reasons:

1. We have no idea what those parameters mean without looking at the datatype definition.
2. We have to pass arguments in a specific order.
3. If the type of the `Env` changes, then this *also* changes.
4. ... but we don't get a break if the field order is changed in a way that respects the types!

Consider swapping the order of `accountId` and `accountPassword` in our data definition.
Now everything breaks mysteriously with no type errors.

Using the function-style for constructing records is probably a bad idea.

# Record Labels

The second most boring way is to use record construction syntax:

```haskell
env :: Env
env = Env
    { accountId = "asdfasdf"
    , accountPassword = "hunter42"
    , requestHook = pure
    , responseHook = pure
    }
```

This solves basically all the problems with function arguments.
However, we're still sensitive to changes in the record constructor.
If we add a new field, we must account for that in all creation sites.
This is annoying, especially since many new fields in records like this are designed to accommodate new functionality or customization, and most existing users want to just ignore them.

# A Default Record

Instead of *constructing* a record, we'll have end users *modify* an existing record.

```haskell
defaultEnv :: Env
defaultEnv = Env
    { accountId = ""
    , accountPassword = ""
    , requestHook = pure
    , responseHook = pure
    }

env :: Env
env = defaultEnv
    { accountId = "asdfasdf"
    , accountPassword = "hunter42"
    }
```

However, this is gross, for a few reasons.
The first is that we provide a dummy value of `accountId` and `accountPassword`, and the end user is required to fill them in.
There's actually no way for us to give a warning or error if they fail to provide it.

The standard solution is to *accept function arguments*, but this has a nasty problem: record syntax binds tighter than anything else, even function application, so we need to do this:

```haskell
defaultEnv :: String -> String -> Env
defaultEnv a p = Env a p pure pure -- brevity, forgive me

env :: Env
env = (defaultEnv "asdfasdf" "hunter42")
    { requestHook = \req -> do
        logRequest req
        pure req
    }
```

That's right - we gotta put parens around our constructor.
We can't use `$` here, either, because the syntax explicitly requires a `value { field0 = val0, ... fieldN = valN }` form.

Also now we're back at the same problem with `defaultEnv` - we can mismatch our function arguments.

# An Args Record

The pattern I chose for [`SqlBackend`](https://hackage.haskell.org/package/persistent-2.13.1.1/docs/Database-Persist-SqlBackend.html#v:mkSqlBackend) in `persistent` is to have an `*Args` record.

```haskell
{-# language DuplicateRecordFields #-}
{-# language RecordWildCards #-}

data EnvArgs = EnvArgs
    { accountId :: String
    , accountPassword :: String
    }

mkEnv :: EnvArgs -> Env
mkEnv EnvArgs {..} = Env
    { requestHook = pure
    , responseHook = pure
    , ..
    }

env :: Env
env = mkEnv EnvArgs 
    { accountId = "asdfasdf"
    , accountPassword = "hunter42"
    }
```

This solves all of the above problems, but it's a bit unsatisfying - we can't *also* modify the `requestHook` and `responseHook` parameters directly in `mkEnv`, we have to do it outside.

```haskell
fullEnv :: Env
fullEnv = 
    (mkEnv EnvArgs {..}) 
        { requestHook = \req -> do
            log req
            pure req
        }
```

Hmm, slightly annoying syntax, again.
But, hey, whatever, it works.

# Codependent Records

No, I'm not talking about some fancy type theory.
Record syntax is essentially *codependent* on the value it is modifying, or the constructor it is using.
We can't pass in a 'record' of stuff and use it in ways that are clever or useful.

Let's talk about the "whitespace operator."
We can imagine defining it like this, for regular functions:

```haskell
( ) :: (a -> b) -> a -> b
f a = f a
```

OK, it's special built in syntax, the definition doesn't make any sense.
But let's try and write it for *records* now.
Remember we need to support update and creation.

```haskell
( ) :: (AllowableRecord con rec result)
    => con -> rec -> result
con rec = implementRecord con rec

class AllowableRecord con rec result where
    implementRecord :: con -> rec -> result
```

Now `rec` is something that can stand alone - it is freed from the codependent relationship with the values and constructors it serves.
What is that something, though?

It could be a row type, like PureScript.
That'd be awesome.

Well now I've just worked myself up into a Mood about GHC's record syntax.
Even with `OverloadedRecordDot`, Haskell's records are still *bad*, they're just not *awful*.

# Ignore Records, Use Functions

This approach eschews records entirely for updates and uses `set*` functions.
It makes for a pretty clean interface.

```haskell
env :: Env
env = 
    addRequestHook (\req -> log req >> pure req)
    $ mkEnv EnvArgs
        { accountId = "asdfasdf"
        , accountPassword = "hunter42"
        }

addRequestHook :: (Request -> IO Request) -> Env -> Env
addRequestHook newHook env = env
    { requestHook = \req -> do
        requestHook env req
        newHook req
    }
```

This is pretty tedious as a library author to write, but it gives you a better interface.

It would be *nice* if we could use this for construction, too.
But this is a challenge because the *type* would change with each new addition to the record.
The `{ ... }` record syntax can know ahead of time how many fields there are, and GHC can issue warnings (or errors) if any are missing.

# Type Changing Updates

We can use a type parameter for each field that is required to be set.

```haskell
data EnvP a b = EnvP
    { accountId :: a
    , accountPassword :: b
    , requestHook :: Request -> IO Request
    , responseHook :: Response -> IO Response
    }

type Env = EnvP String String

data Void

defaultEnv :: EnvP Void Void
defaultEnv = EnvP
    { requestHook = pure
    , responseHook = pure
    }
```

GHC will issue warnings here, but that's okay - we *know* they're undefined at the type level.

Now we can write our `set` functions:

```haskell
setAccountId :: String -> EnvP a b -> EnvP String b
setAccountId str env = env { accountId = str }

setAccountPassword :: String -> EnvP a b -> EnvP a String
setAccountPassword str env = env { accountPassword = str }

env :: Env
env = 
    setAccountId "asdfasdf" 
    $ setAccountPassword "hunter42"
    $ defaultEnv
```

And, well, this actually works out.
If we only expose the `Env` type (and maybe a pattern synonym for construction/deconstruction), this interface should be pretty safe and straightforward.
A final `mkEnv` call could even put it behind a `newtype` wrapper, or a similar datatype, similar to the `*Args` pattern above.
The boilerplate sucks, but would be easy to `TemplateHaskell` away.

Can `OverloadedRecordDot` help us here?
With some of the tricks in [Stealing `impl` From Rust](https://www.parsonsmatt.org/2021/07/29/stealing_impl_from_rust.html), sort of.
We can write simple setters:

```haskell
data User = User { name :: String }

instance HasField "setName" User (String -> User) where
    getField self newName = 
        self { name = newName }
```

And, using the One Weird Trick to defeat functional dependencies, we can write type-changing setters, too!

```haskell
instance 
    HasField "setAccountId" (EnvP a b) (x -> EnvP x b)
  =>
    HasField "setAccountId" (EnvP a b) (x -> EnvP x b)
  where
    getField self x = self { accountId = x }
```

Now, to provide a good UX, we'd want to require this be `String`, possibly with a nice `TypeError` constraint that complains.
But this'll work for now - we can totally write this:

```haskell
env :: EnvP String Void
env = defaultEnv.setAccountId "asdfasdf"
```

Unfortunately, *chaining* this isn't really feasible.

```haskell
env :: EnvP String String
env = defaultEnv.setAccountId "asdfasdf".setAccountPassword "hunter42" 
```

This fails with an error, as `.setAccountPassword` is attaching to `"asdfasdf"`, not the *return* of `defaultEnv.setAccountId "asdfasdf"`.
So we can work around this with parens:

```haskell
env :: EnvP String String
env = 
    (defaultEnv.setAccountId "asdfasdf").setAccountPassword "hunter42" 
```

This gets annoying, especially as the chaining goes up.
Assigning to intermediate values also works:

```haskell
env :: EnvP String String
env = 
    let 
        withId = defaultEnv.setAccountId "asdfasdf"
        withPassword = withId.setAccountPassword "hunter42" 
     in 
        withPassword
```

But, at this point, I'm wondering how this is any better than just writing

```haskell
env :: EnvP String String
env = setAccountId "asdfadsf" $ setAccountPassword "hunter42" defaultEnv
```

Unfortunately, the type errors can get a bit weird and annoying carrying around the `EnvP` value.
Wrapping it in a `newtype` or translating to a separate data structure can make errors better.
It also distinguishes the "create this record" and "use this record" scenarios.

# Back to Args

And, yeah, ultimately, I think `Args` is probably the right way to go.

There's not really much to a library for it.
You'd define a class like this:

```haskell
class New a where
    type Args a = r | r -> a
    
    new :: Args a -> a
```

You want the `TypeFamilyDependencies` annotation on `Args` because you want the argument type to inform the result type.
A data family would also work, but it would not allow you to define it separately and document it with a separate type name.
Maybe a problem, maybe not.
It may also be nice to vary the return type, allowing `IO`, for example.
That looks like this:

```haskell
class New a where
    type Args a = r | r -> a
    type Return a = r | r -> a
    type Return a = a

    new :: Args a -> Return a
```

But now we've just, got, like, this type class, where it takes a thing, and returns another thing (maybe in IO, maybe not?? who knows).
And this is so general and lawless that making a library for it seems a bit silly.

So, instead of writing a library, I wrote a blog post.
