---
title: "Mirror Mirror: Reflection and Encoding Via"
date: 2020-02-04
layout: post
categories: programming
---

Mirror, mirror, on the wall,

where is the skolem that escapes the `forall`?

This post is about reflection, reification, and (to get to the pragmatism) the use of the new `DerivingVia` mechanism to provide awesome codecs.
What does reflection and reification have to do with any of this?
Well, we'll see, but first let's dig into some code.

Encoding and decoding JSON is a common problem, and you very often need to massage the data a little bit in order to get what you want.
Sometimes you need to maintain backwards compatibility with old services, and this means that you can't just do whatever you want internally.
What works best for your domain and codebase doesn't necessarily play nicely with the boilerplate reducing deriving mechanisms or metaprogramming.

You can dispense with type classes and generic deriving.
Writing encoders and decoders by hand is a great and declarative solution, and is often the right answer.
However, the work can be boilerplate-y and error-prone, and some machine help is much appreciated.

Fortunately, [`DerivingVia`](https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf) can be used to handle much of this work safely, composably, and without boilerplate.
Let's dig into what I've been working on.

We're going to need a boatload of language extensions to make this work.

```haskell
{-# LANGUAGE TypeOperators, AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
```

Don't worry about them if you don't understand them.
For some more boilerplate, we're going to define the most common domain type: `User`.

```haskell
data User = User
    { userName           :: String
    , userAge            :: Int
    , userFavoriteAnimal :: String
    }
    deriving (Show, Generic)

bob :: User
bob = User "Bob" 32 "cats"
```

Now, `User` does *not* have a `ToJSON` instance.
But we want to convert it to JSON anyway.
We can write a `newtype` wrapper that delegates to the `Generic` stuff with JSON, as a way to provide a `ToJSON` instance for a type that doesn't have one.

```haskell
newtype GenericToJSON value = GenericToJSON value

instance ToJSON (GenericToJSON value) where
    toJSON (GenericToJSON value) =
        genericToJSON defaultOptions value
```

GHC is definitely *not* going to like this, because we need some constraints.
So let's have GHC compile this and complain!

```
/home/matt/Projects/encoding-via/src/Lib.hs:74:24: error:
    • No instance for (Generic a) arising from a use of ‘genericToJSON’
      Possible fix:
        add (Generic a) to the context of the instance declaration
    • In the expression: genericToJSON defaultOptions a
      In an equation for ‘toJSON’:
          toJSON (GenericToJSON a) = genericToJSON defaultOptions a
      In the instance declaration for ‘ToJSON (GenericToJSON a)’
   |
74 |     toJSON (GenericToJSON a) = genericToJSON defaultOptions a
   |                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Let's follow GHC's suggestion:

```haskell
instance Generic a => ToJSON (GenericToJSON a) where
    toJSON (GenericToJSON a) = genericToJSON defaultOptions a
```

Now we get another error:

```
/home/matt/Projects/encoding-via/src/Lib.hs:75:24: error:
    • Could not deduce (aeson-1.4.6.0:Data.Aeson.Types.ToJSON.GToJSON
                          Value Zero (Rep a))
        arising from a use of ‘genericToJSON’
      from the context: Generic a
        bound by the instance declaration at src/Lib.hs:(72,5)-(73,32)
    • In the expression: genericToJSON defaultOptions a
      In an equation for ‘toJSON’:
          toJSON (GenericToJSON a) = genericToJSON defaultOptions a
      In the instance declaration for ‘ToJSON (GenericToJSON a)’
   |
75 |     toJSON (GenericToJSON a) = genericToJSON defaultOptions a
   |                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Another type class constraint to paste in.
However, there's something tricky here:
GHC is reporting a fully qualified name for the `GToJSON` class.
That means it isn't in scope.
Let's [Hoogle the `GToJSON` class](https://www.stackage.org/lts-14.22/hoogle?q=GToJSON).
Looks like there are two types here with the same name.
We've got `type GToJSON = Internal.GToJSON Value`.
So I think we can just use that in the constraint:

```haskell
instance 
    (Generic a, GToJSON Zero (Rep a)) 
  => 
    ToJSON (GenericToJSON a) 
  where
    toJSON (GenericToJSON a) = genericToJSON defaultOptions a
```

Sure enough, this compiles!
Can we use it to convert a `User` to JSON, now?
Yes.

```haskell
>>> BS8.putStrLn (Aeson.encode (GenericToJSON bob))
{"userName":"Bob","userAge":32,"userFavoriteAnimal":"cats"}
```

OK, that may not be the encoding we want, but it does work.

# DerivingVia

OK, OK, but now we actually need to provide a `ToJSON` instance for the `User`.
We have a bunch of options:

### Manual

BORING

```haskell
instance ToJSON User where
    toJSON user = object
        [ "userName" =. userName user
        , "userAge" =. userAge user
        , "userFavorateAnimal" =. userFavoriteAnimal user
        ]
```

Also, HWOOPS, you may have noticed the typo.
That's unfortunately already been released and is now part of the Public API which we will embarrasingly support for the next decade or two.
`Referer` has some company, at least.

Anyway this is  boring, error-prone, and full of repetition.
But!
Importantly, it gives us a tremendous amount of *control* over the representation.
We specify exactly what we want and how we want it.
Want to special case a field name?
Easy! Just write it.
Want to special case a value representation?
Easy! Just do it.

### DeriveAnyClass

All you have to do is throw a deriving clause on `User` for this to work out.

```haskell
data User = User ...
    deriving (Generic, ToJSON)
```

This is easy. 
But it requires a lot of work from GHC and the library author.
GHC needs a feature to permit library authors to provide specialized defaults for type class methods, and library authors must then provide those specialized defaults.
Library authors have to pick a single default set that is privileged for `DeriveAnyClass`, which is unfortunate.

Fortunately for users, this is very easy.
There's nothing to it.
But we also don't have any control over it.
So let's look at a slightly more flexible way:

### Generic Deriving

```haskell
instance ToJSON User where
    toJSON = genericToJSON defaultOptions
```

The value `defaultOptions` gives us tools and hooks to modify field labels and constructor values and other ways that the JSON encoding can be handled.
This is good and convenient.
The [`aeson-casing`](https://hackage.haskell.org/package/aeson-casing) gives us a function `snakeCase` that we can use to snake case the fields instead of using the text of the field that we're given.

```haskell
instance ToJSON User where
    toJSON = genericToJSON options
      where
        options = defaultOptions
            { fieldLabelModifier = 
                \fieldLabel -> snakeCase fieldLabel
            }

>>> BS8.putStrLn (Aeson.encode bob)
{"user_name":"Bob","user_age":32,"user_favorite_animal":"cats"}
```

Cool.
And finally we can drop the type name, because we want prettier fields.

```haskell
instance ToJSON User where
    toJSON = genericToJSON options
      where
        options = defaultOptions
            { fieldLabelModifier = 
                \fieldLabel -> snakeCase (drop (length "user") fieldLabel)
            }

>>> BS8.putStrLn (Aeson.encode bob)
{"name":"Bob","age":32,"favorite_animal":"cats"}
```

Nice. That's what we want.

### DerivingVia

We can derive a Generic-based instance using our `newtype` from earlier:

```haskell
data User = User
    { userName           :: String
    , userAge            :: Int
    , userFavoriteAnimal :: String
    }
    deriving (Show, Generic)
    deriving ToJSON via GenericToJSON User
```

The `via` keyword allows us to specify a `newtype` wrapper that might contain additional information to use in deriving.
This will generate an instance that looks like this:

```haskell
instance ToJSON User where
    toJSON user = toJSON (coerce user :: GenericToJSON User)
```

Basically, we're delegating to this instance under-the-hood:

```haskell
instance 
    (Generic a, GToJSON Zero (Rep a)) 
  => 
    ToJSON (GenericToJSON a) 
  where
    toJSON (GenericToJSON a) = genericToJSON defaultOptions a
```

Here's what I find awesome about this:

- It subsumes default methods *and* democratizes them - library authors are no longer *required* to provide these default methods, and library users can supply them as well.
- Because it uses type classes, it is completely canonical - there can only be one instance for a type, and it should be pretty easy to find either the type or the instance.
- We can customize and reuse these values easily

Indeed, `GenericToJSON` is too strict of a name - we can use that wrapper for *anything* that just delegates to the Generic instance.
This type is canonically available as [`Generically`](https://www.stackage.org/haddock/lts-14.22/generic-data-0.7.0.0/Generic-Data.html#t:Generically).

### DerivingVia + Customization?

But, how can we customize?

If we write the instance by hand, then we can customize the `options` passed in.
But the language in `DerivingVia` doesn't allow for mere values - only types can be talked about.

Fortunately, we have ways of communicating across the type-value divide.

# Functions on Values and Types

In Haskell, we are very familiar with functions from values to values.
It's functional programming!

But we also have types.
Can we have functions from values to types?
What about functions from types to types?
Or functions from types to values?

Value-to-value functions are ordinary functions.
And we have type-to-type functions using `TypeFamilies`.
Value-to-type functions are the realm of dependent types, and Haskell can only sorta simulate these sometimes in a limited and weird way.

But we want type-to-value functions.
Given a type, return a value.
We have these - they are called "type classes."

## what??

It's a bit of a mindbender! For sure.
And the syntax is a little awkward.
Don't worry.
Let's make a type class that make this super evident.

```haskell
class TypeToInt a where
    typeToInt :: Int
```

The class `TypeToInt` is a function that accepts a type and provides a value.
We can define an instance like this:

```haskell
instance TypeToInt Int where
    typeToInt = 1

instance TypeToInt String where
    typeToInt = 2

instance TypeToInt Char where
    typeToInt = 3
```

We can use the type function like this

```haskell
>>> typeToInt @Int
1
>>> typeToInt @Char
3
```

The `@` is a `TypeApplications` syntax - it allows us to explicitly pass the type to the value.

Typical type classes, like `Monoid`, are similar.
Consider `mempty` - it's a value, all alone.
If we use it unadorned, it looks like this:

```haskell
mempty :: (Monoid a) => a
```

If we view this as a function from types to values, then we can pass a type and receive a value:

```haskell
>>> mempty @(Sum Int)
Sum { getSum = 0 }
```

Anyway, to get back on track, we're going to need to build a type-level language for modifying JSON options, and then we're going to need to use type classes to get a value level modifier.
If that sounds scary, then, well, it kind of is.
But no worries - you'll get the hang of it!

# Modify Options

```haskell
newtype
    Codec
        (tag :: k)
        (val :: Type)
      =
        Codec val
```

The type that we'll use to hang our hat is this.
The `Codec` type takes a type parameter `tag` that can be of any kind `k`, and it contains a single value of type `val`.
This allows us to use it with `DerivingVia`.

Now, we'll define an instance of `ToJSON` for `Codec`, which modifies the options based on `tag`.

```haskell
instance 
    ( GToJSON Zero (Rep a), Generic a
    , ModifyOptions tag
    )
  =>
    ToJSON (Codec tag a)
  where
    toJSON (Codec a) = 
        genericToJSON (modifyOptions @tag defaultOptions) a
```

`ModifyOptions` is a function from a type to a value - in this case, a function which modifies options.
We'll start with the base case - do nothing!
For this, we can use the `()` type, but we'll alias it for readability:

```haskell
type AsIs = ()

class ModifyOptions tag where
    modifyOptions :: Options -> Options

instance ModifyOptions AsIs where
    modifyOptions = id
```

This gives us the same thing as `deriving ToJSON via Generically User`, and we can verify this:

```haskel
>>> encode (Codec bob :: Codec AsIs User)
{"userName":"Bob","userAge":32,"userFavoriteAnimal":"cats"}
```

Now, we want the ability to `snake_case` the options.
So we'll create a type:

```haskell
data SnakeCase
```

The purpose of this type is to "reflect" the value `snakeCase :: String -> String` and modify the field labels with that function.

```haskell
instance ModifyOptions SnakeCase where
    modifyOptions options = options
        { fieldLabelModifier = \fieldLabel ->
            snakeCase (fieldLabelModifier options fieldLabel)
        }
```

Oof, record update, how nasty. Let's factor that out into it's own pattern:
we want to take an `Options` and compose a function with the existing `fieldLabelModifier`.

```haskell
addFieldModifier :: (String -> String) -> Options -> Options
addFieldModifier f options = options
    { fieldLabelModifier = f . fieldLabelModifier options
    }

instance ModifyOptions SnakeCase where
    modifyOptions = addFieldLabelModifier snakeCase
```

Much nicer.
Excellent.
Does this work?
Let's try!

```haskell
>>> BS8.putStrLn (Aeson.encode (Codec bob :: Codec SnakeCase User))
{"user_name":"Bob","user_age":32,"user_favorite_animal":"cats"}
```

Nice.

Now, let's drop that type name from the front.
We'll write a combinator that lets you specify that you want to `Drop` something from the front.

```haskell
data Drop something
```

And, here's our instance:

```haskell
instance (KnownSymbol symbol) => ModifyOptions (Drop symbol) where
    modifyOptions = 
        addFieldLabelModifier $ \fieldLabel ->
            case List.stripPrefix prefix fieldLabel of
                Just stripped ->
                    stripped
                Nothing ->
                    fieldLabel
      where 
        prefix = symbolVal (Proxy @symbol)
```

There's a bit to unpack here.
This type class is matching on *two* types: one visibly (`Drop symbol`), and one invisibly.
It's matching on the inferred *kind* of `symbol` -- `symbol :: Symbol`.
It's real easy to get tripped up when GHC starts inferring stuff about kinds, so if you get confused here, you're in good company - this stuff confuses me all the time.

A `Symbol` is a String at the type level.
The function [`symbolVal`](https://www.stackage.org/haddock/lts-14.22/base-4.12.0.0/GHC-TypeLits.html#v:symbolVal) is used to get a `String` from a `Symbol`.
It's another function from types to values that we've been using.
So we'd say that we're "reflecting" the symbol into the `prefix` variable, and then using it normally.

This works!

```haskell
>>> BS8.putStrLn (Aeson.encode (Codec bob :: Codec (Drop "user") User))
{"Name":"Bob","Age":32,"FavoriteAnimal":"cats"}
```

But we want to do *both* of these at the same time, without writing a bunch of boilerplatey code.

# Composing

We need a type to compose these functions.
We can't use `.` as a type operator.
So that leaves us with `$` and `&`.
`$` has a useful type operator already - you can feasibly use it to write `IO $ Either String Char` and remove brackets there.
So we'll use `&`.

```haskell
data a & b

infixr 6 &
```

Now, we'll write an instance of `ModifyOptions` for this type.

```haskell
instance
    ()
  =>
    ModifyOptions (a & b)
  where
    modifyOptions = undefined
```

Just kidding, we put in a dummy/skeleton implementation.
So the idea is that we want to have a symmetry with `&`, which is defined like:

```haskell
(&) :: a -> (a -> b) -> b
a & f = f a
```

You use it like `[1,2,3] & map (+1)`.
It's similar to Elm, F#, and Elixir's `|>` operator.

With this understanding, we can stitch together the instance.
We need for `a` and `b` to have an instance of `ModifyOptions`, and then we'll compose those functions.

```haskell
instance
    (ModifyOptions a, ModifyOptions b)
  =>
    ModifyOptions (a & b)
  where
    modifyOptions = modifyOptions @b . modifyOptions @a
```

Now, we can write our `Codec` that will do both of these operations.

```haskell
>>> let val = Codec bob :: Codec (Drop "user" & SnakeCase) User
>>> BS8.putStrLn (Aeson.encode val)
{"name":"Bob","age":32,"favorite_animal":"cats"}
```

Armed with this, we can now *derive* that instance:

```haskell
data User = User ...
    deriving stock
        Generic
    deriving 
        ToJSON
      via
        Codec (Drop "user" & SnakeCase) User
```

# DerivingVia: Reflecting Types to Values to Control Classes

`DerivingVia` gives us a powerful language for deriving instances, but it requires that we write at the type level.
Fortunately, we can reflect our types into functions, and use those to drive behavior.

These `newtype` wrappers aren't useful *only* for deriving.
We can also use it to specify *alternative* behaviors easily.
We've needed this recently at my company to load results in a JSONB array.

Postgresql has an aggregation function [`jsonb_agg`](https://www.postgresql.org/docs/9.5/functions-aggregate.html) that will take an expression, convert it to JSONB, and collect the results in a JSONB list.
However, there's no way to control the JSONB representation - `postgresql` uses the column names for the keys, as-is.

`persistent` can automatically derive JSON instances for you, but it can potentially pick different encoder/decoder than what postgresql uses.
This is the default behavior with most of the settings.

Furthermore, you may not even *have* derived JSON instances for these types!
So how are you going to make the communication work, without a ton of error-prone boilerplate?

We'll use the exact same `newtype` and reflection tricks.
We'll point these techniques at `FromJSON` instead, which should be able to reuse *all* of the combinators we're building here to modify the requisite options.

In the interest of brevity, though, *that* particular exposition will have to wait for another post.
In the meantime, you can look at code on my [`encoding-via`](https://github.com/parsonsmatt/encoding-via/blob/master/src/Lib.hs) repository.
